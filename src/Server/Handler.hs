{-# LANGUAGE CPP #-} 
module Server.Handler where

import           Agda                           ( getCommandLineOptions
                                                , runAgda
                                                )
import qualified Agda.IR                       as IR
import           Agda.Interaction.Base          ( CommandM
                                                , CommandQueue(..)
                                                , CommandState(optionsOnReload)
                                                , Rewrite(AsIs)
                                                , initCommandState
                                                )
import           Agda.Interaction.BasicOps      ( atTopLevel
                                                , typeInCurrent
                                                )
import qualified Agda.Interaction.Imports      as Imp
import           Agda.Interaction.InteractionTop
                                                ( cmd_load'
                                                , localStateCommandM
                                                )
import           Agda.Interaction.Options       ( CommandLineOptions
                                                  ( optAbsoluteIncludePaths
                                                  )
                                                )
import qualified Agda.Parser                   as Parser
import           Agda.Position                  ( makeToOffset
                                                , toAgdaPositionWithoutFile
                                                )
import           Agda.Syntax.Abstract.Pretty    ( prettyATop )
import           Agda.Syntax.Parser             ( exprParser
                                                , parse , moduleParser , parseFile
                                                )
import           Agda.Syntax.Concrete             ( Module(..),Declaration(..))
import           Agda.Syntax.Parser.Tokens      ( Token(..))
import           Agda.Syntax.Translation.ConcreteToAbstract
                                                ( concreteToAbstract_ , TopLevel(..),TopLevelInfo(..))
import           Agda.TypeChecking.Monad        ( HasOptions(commandLineOptions)
                                                , setInteractionOutputCallback
                                                )
import           Agda.TypeChecking.Warnings     ( runPM )
import qualified Agda.Syntax.Position as APosition
#if MIN_VERSION_Agda(2,6,4)
import           Agda.Syntax.Common.Pretty      ( render )
#else
import           Agda.Utils.Pretty              ( render )
#endif
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( LspM )
import qualified Language.LSP.VFS              as VFS
import           Monad
import           Options                        ( Config(..)
                                                , Options(optRawAgdaOptions),empty
                                                , LoadedFileInfo(..)
                                                )
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Types            as LSP
import qualified Server.Index as Index


import qualified Data.Strict as Strict 
import qualified Data.Map as Map 
import qualified Language.LSP.Types as LSPTypes 
import Agda.Utils.FileName
import Agda.Interaction.FindFile
import qualified Agda.Syntax.Common.Aspect as Aspect
import qualified Language.LSP.Server as LSPServer


import qualified Agda.TypeChecking.Errors as TCM
import qualified Agda.TypeChecking.Monad  as TCM
import qualified Agda.TypeChecking.Monad.Base  as TCM
import qualified Agda.TypeChecking.Pretty as TCM
import qualified Agda.Interaction.Highlighting.Generate as G
import Agda.Utils.List            ( caseList, last1 )

import qualified Server.Highlight as H
import qualified Agda.Interaction.Base as CM
import qualified Agda.Interaction.Highlighting.Precise as P
import qualified Agda.Interaction.Highlighting.Range as A
import qualified Server.Parsing as Parsing
import qualified Data.Array as Array
import Data.Array ((!))
import Agda.Syntax.Parser                   as P

initialiseCommandQueue :: IO CommandQueue
initialiseCommandQueue = CommandQueue <$> newTChanIO <*> newTVarIO Nothing

runCommandM :: CommandM a -> ServerM (LspM Config) (Either String a)
runCommandM program = do
  env <- ask
  runAgda $ do
    -- get command line options
    options <- getCommandLineOptions

    -- we need to set InteractionOutputCallback else it would panic
    lift $ setInteractionOutputCallback $ \_response -> return ()

    -- setup the command state
    commandQueue <- liftIO initialiseCommandQueue
    let commandState = (initCommandState commandQueue)
          { optionsOnReload = options { optAbsoluteIncludePaths = [] }
          }

    lift $ evalStateT program commandState

inferTypeOfText
  :: FilePath -> Text -> ServerM (LspM Config) (Either String String)
inferTypeOfText filepath text = runCommandM $ do
    -- load first
  cmd_load' filepath [] True Imp.TypeCheck $ \_ -> return ()
  -- infer later
  let norm = AsIs
  -- localStateCommandM: restore TC state afterwards, do we need this here?
  typ <- localStateCommandM $ do
#if MIN_VERSION_Agda(2,6,3)
    (e, _attrs)
#else
    e
#endif
       <- lift $ runPM $ parse exprParser (unpack text)
    lift $ atTopLevel $ do
      concreteToAbstract_ e >>= typeInCurrent norm

  render <$> prettyATop typ

onHover :: LSP.Uri -> LSP.Position -> ServerM (LspM Config) (Maybe LSP.Hover)
onHover uri pos = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing   -> return Nothing
    Just file -> do
      let source      = VFS.virtualFileText file
      let offsetTable = makeToOffset source
      let agdaPos     = toAgdaPositionWithoutFile offsetTable pos
      lookupResult <- Parser.tokenAt uri source agdaPos
      case lookupResult of
        Nothing             -> return Nothing
        Just (_token, text) -> do
          case LSP.uriToFilePath uri of
            Nothing       -> return Nothing
            Just filepath -> do
              let range = LSP.Range pos pos

              inferResult <- inferTypeOfText filepath text
              case inferResult of
                Left err -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent
                        "agda-language-server"
                        ("Error: " <> pack err)
                  return $ Just $ LSP.Hover content (Just range)
                Right typeString -> do
                  let content = LSP.HoverContents $ LSP.markedUpContent
                        "agda-language-server"
                        (pack typeString)
                  return $ Just $ LSP.Hover content (Just range)



onDocumentChanged :: LSP.Uri -> LSP.List LSP.TextDocumentContentChangeEvent -> ServerM (LspM Config) ()
onDocumentChanged ur changes@(LSP.List changesList) = do
    case LSP.uriToFilePath ur of 
        Nothing -> return ()
        Just path -> do
            c <- LSPServer.getConfig
            vfile <- LSP.getVirtualFile (LSP.toNormalizedUri ur)
            case vfile of 
                Nothing -> return ()
                Just vvfile -> do
                    let text = VFS.virtualFileText vvfile
                    --let text = pack "suc zero ( )\nSet + "
                    let textLines = T.lines text
                    (AbsolutePath fp) <- liftIO $ absolute path
                    let pathString = unpack fp
                    if Map.member pathString (loadedFiles c) then do
                           let (Just file) = Map.lookup pathString (loadedFiles c)
                           let fileInfo@(LoadedFileInfo hi tok asp) = file
                           let stringContent = (unpack text)
                           let content = (Array.listArray (0 , Text.length text) stringContent)
                           let newIndex = Index.stringToLineIndex stringContent
                           let newFile@(LoadedFileInfo nHi@(LSP.List lnHi) ntok nasp) = (updateLoadedFile fileInfo newIndex content changes)
                           LSPServer.setConfig (c {loadedFiles = Map.insert pathString newFile (loadedFiles c)  })
                           let change@(LSP.TextDocumentContentChangeEvent startRange@(Just (LSP.Range s e)) _ _) = (head changesList)
                           let changeRange = (LSP.Range (LSP.Position (LSP._line s) 0) (LSP.Position ((LSP._line s) + lineDiff change + 2) 0))
                           hi3 <- liftIO $ runPMIO $ weaveNewTokens newIndex tok content changeRange 0 0 lnHi 
                           --hi3 <- liftIO $ runPMIO $ weaveNewTokens newIndex tok content changeRange 1 0 [1,0,4,12,0]
                           case hi3 of 
                                (Left a,_) -> return ()
                                (Right newTokens,_) -> do
                                   let front@(LSP.TextDocumentContentChangeEvent (Just range) _ text) = head changesList
                                   LSPServer.setConfig (c {loadedFiles = Map.insert pathString (newFile {hi = (LSP.List newTokens) })(loadedFiles c)  })
                                   return ()
                    else 
                        return ()

onHighlight :: LSP.Uri -> ServerM (LspM Config) (Maybe LSP.SemanticTokens)
onHighlight ur =
  case LSP.uriToFilePath ur of 
    Nothing -> return Nothing
    Just path -> do 
      c <- LSPServer.getConfig
      (AbsolutePath fp) <- liftIO $ absolute path
      let pathString = unpack fp
      if Map.member pathString (loadedFiles c) then do
             let (Just file) = Map.lookup pathString (loadedFiles c)
             let (LoadedFileInfo hi tok asp) = file
             return (Just (LSP.SemanticTokens Nothing hi))
      else do
             loadFile ur
             c <- LSPServer.getConfig
             if Map.member pathString (loadedFiles c) then do
                 let (Just file) = Map.lookup pathString (loadedFiles c)
                 let (LoadedFileInfo hi tok asp) = file
                 return (Just (LSP.SemanticTokens Nothing hi))
             else 
                return Nothing



--getTokenMap :: LSP.List LSP.UInt -> Text -> Map.Map String Int
--getTokenMap _ _ = Map.empty

--updateHighlighting :: LSP.List LSP.UInt -> LSP.TextDocumentContentChangeEvent -> LSP.List LSP.UInt
--updateHighlighting _ _ = LSP.List []
pointIn :: LSP.UInt -> LSP.UInt -> LSP.Range -> Bool
pointIn line col range@(LSP.Range start end) | LSP._line start == LSP._line end = (line == LSP._line start) && (col >= LSP._character start) && (col < LSP._character end)
                                             | line == LSP._line start =  col >= LSP._character start
                                             | line > LSP._line start && line < LSP._line end = True
                                             | line == LSP._line end = col < LSP._character end
                                             | otherwise = False

rangeIn :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.Range -> Bool
rangeIn line col length range@(LSP.Range start end) =
    let tokenRange = (LSP.Range (LSP.Position line col) (LSP.Position line (col+length))) in
        ((pointIn line col range) || (pointIn line (col + length) range)) || ( (pointIn (LSP._line start) (LSP._character start) tokenRange) ||
            (pointIn (LSP._line start) (LSP._character end) tokenRange))

after :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.Range -> Bool
after line col length range@(LSP.Range start end) = (not (rangeIn line col length range)) && 
            (line > LSP._line end ||  (line == LSP._line end &&  col >= LSP._character end))

lineCount :: Text -> LSP.UInt
lineCount text = do
    let textLines = T.splitOn "\n" text
    let lineCount = length textLines
    if lineCount == 0 then
        0
    else
        fromIntegral $ lineCount - 1
colCount :: Text -> LSP.UInt
colCount text = do
    let textLines = T.splitOn "\n"  text
    let lineCount = length textLines
    if lineCount == 0 then
        0
    else
        fromIntegral $ T.length (last textLines)
lineDiff :: LSP.TextDocumentContentChangeEvent  -> LSP.UInt
lineDiff e@(LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text) = (LSP._line start - LSP._line end) + lineCount text
lineDiff _ = 0
colDiff :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.TextDocumentContentChangeEvent -> LSP.UInt
colDiff preColDistance line col e@(LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text) 
            | line > LSP._line end = 0
            | addedLines >= 1 =  (- LSP._character end) + colCount text  --preColDistance not needed
            | LSP._line start == LSP._line end  = colCount text - (LSP._character end - LSP._character start)
            | otherwise = preColDistance + (- LSP._character end) + colCount text  
      where 
        addedLines = lineCount text
colDiff _ _ _ _ = 0

updateAspectMapImpl :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.UInt -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)] -> LSP.TextDocumentContentChangeEvent -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)]
updateAspectMapImpl eLine eCol tLine tCol pcolDistRange (head@(dLine,dCol,cLength,a):tail) e@(LSP.TextDocumentContentChangeEvent (Just range@(LSP.Range start end)) _ _ ) = do
    let newLine = tLine + dLine
    let pnewCol = tCol + dCol
    let neCol = if dLine == 0 then eCol else 0
    let newCol = if dLine == 0 then pnewCol else dCol
    let newColDistRange =if LSP._line start == tLine then LSP._character start - newCol else pcolDistRange
    if rangeIn newLine newCol cLength range then 
        updateAspectMapImpl (dLine + eLine ) (dCol + neCol) newLine newCol pcolDistRange tail e
    else
        if after newLine newCol cLength range then
            (dLine+(eLine+ lineDiff e)  , dCol+(neCol + colDiff pcolDistRange newLine newCol e),cLength,a) : tail
        else
            head : updateAspectMapImpl 0 0 newLine newCol newColDistRange tail e
updateAspectMapImpl _ _ _ _ _ _ _ = []

updateAspectMap :: [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)] -> LSP.TextDocumentContentChangeEvent -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)]
updateAspectMap l e@(LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ _) = updateAspectMapImpl 0 0 0 0 (LSP._character start) l e
updateAspectMap _ _ = []

updateSemanticTokens :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.UInt -> [LSP.UInt]  -> LSP.TextDocumentContentChangeEvent -> [LSP.UInt]
updateSemanticTokens eLine eCol tLine tCol pcolDistRange (dLine:dCol:cLength:x1:x2:tail) e@(LSP.TextDocumentContentChangeEvent (Just range@(LSP.Range start end)) _ _ ) = do
    let newLine = tLine + dLine
    let pnewCol = tCol + dCol
    let neCol = if dLine == 0 then eCol else 0
    let newCol = if dLine == 0 then pnewCol else dCol
    let newColDistRange =if LSP._line start == tLine then LSP._character start - newCol else pcolDistRange
    if rangeIn newLine newCol cLength range then 
        updateSemanticTokens (dLine + eLine ) (dCol + neCol) newLine newCol pcolDistRange tail e
    else
        if after newLine newCol cLength range then
             dLine+(eLine+ lineDiff e) :(dCol+(neCol + colDiff pcolDistRange newLine newCol e)):cLength:x1:x2 : tail
        else
            dLine:dCol:cLength:x1:x2:updateSemanticTokens 0 0 newLine newCol newColDistRange tail e
updateSemanticTokens _ _ _ _ _ _ _ = []



updateSemanticTokensMap :: [LSP.UInt] -> LSP.TextDocumentContentChangeEvent -> [LSP.UInt]
updateSemanticTokensMap l e@(LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ _) = updateSemanticTokens 0 0 0 0 (LSP._character start) l e
updateSemanticTokensMap _ _ = []


nextWhitespacePosition :: Array.Array Int Char -> Int -> Int
nextWhitespacePosition content offset | offset >= snd (Array.bounds content) = snd (Array.bounds content)
                                      | otherwise = if (content Array.! offset) == '\r' || (content Array.! offset) == '\n' then offset else
                                                nextWhitespacePosition content (offset + 1)
lineEnd :: Index.LineIndex -> Array.Array Int Char -> Int -> Int
lineEnd index content line = do
    let offset = Index.positionToOffset index (LSP.Position (fromIntegral line) 0)
    nextWhitespacePosition content offset 

intervallToString :: Index.LineIndex -> Array.Array Int Char -> LSP.Range -> String
intervallToString index content (LSP.Range start end) = do
    let oStart = (Index.positionToOffset index start)
    let oEnd = (Index.positionToOffset index end)
    Parsing.offsetsToString content oStart oEnd

encodeTokens :: LSP.UInt -> LSP.UInt -> [(LSP.Range,LSP.UInt)] -> [LSP.UInt]
encodeTokens tLine tCol (((LSP.Range start end),tokenType):tail) = do
    let dCol = (LSP._character start)-tCol 
    0:dCol:(LSP._character end - LSP._character start):tokenType:0:(encodeTokens tLine (tCol+dCol) tail)
encodeTokens tLine tCol [] = []

weaveNewTokens :: Index.LineIndex -> Map.Map String LSP.UInt -> Array.Array Int Char -> LSP.Range -> LSP.UInt -> LSP.UInt -> [LSP.UInt] -> PM [LSP.UInt]
weaveNewTokens index tmap content range tLine tCol (dLine:dCol:cLength:x1:x2:tail) 
        | not ((rangeIn  tLine tCol 1 range) || (rangeIn newLine tLine 1 range) || ((after  newLine newCol 1 range) && not (after tLine tCol 1 range)))  = do
                    tailResult <- (weaveNewTokens index tmap content range (tLine+dLine) (if tLine == 0 then tCol+dCol else dCol) tail)
                    return (dLine:dCol:cLength:x1:x2:tailResult)
        | otherwise = do
    let lastCol = if dLine == 0 then tCol + dCol else fromIntegral $ lineEnd index content (fromIntegral tLine)
    tokens <- Parsing.parseTokenIntervals index content tLine tCol lastCol
    let newTokens = map (\(Just x) -> x) (filter (/= Nothing ) (map (\x -> do 
            let tokenString = intervallToString index content x 
            let member = Map.lookup tokenString tmap 
            case member of
                Just tokenType -> (Just (x,tokenType)) 
                Nothing ->  Nothing) tokens))
    if length newTokens == 0 then do
            tailResult <- weaveNewTokens index tmap content range newLine newCol tail
            return (dLine:dCol:cLength:x1:x2:tailResult) else  do
        let extraTokens = encodeTokens tLine tCol newTokens
        tailResult <- weaveNewTokens index tmap content range (tLine + dLine) newCol tail
        let modifiedCol = if dLine == 0 then dCol - ((\(LSP.Range f s) -> (LSP._character f)) (fst (last newTokens)) - tCol) else dCol
        return (extraTokens <> (dLine:modifiedCol:cLength:x1:x2:tailResult))
    where 
        newCol = if dLine == 0 then tCol + dCol else dCol
        newLine = tLine+dLine

weaveNewTokens index tmap content _ _ _ _ = return []

updateLoadedFile :: LoadedFileInfo -> Index.LineIndex -> (Array.Array Int Char) -> LSP.List LSP.TextDocumentContentChangeEvent -> LoadedFileInfo
updateLoadedFile (LoadedFileInfo (LSP.List hi) tok asp) index content (LSP.List updates) = do
          let newAspectPositions = foldl updateAspectMap asp updates
          --let hi2 = H.convertRangeTokens 0 0 H.defaultTokenMap newAspectPositions
          let hi2 = foldl updateSemanticTokensMap hi updates
          --hi3 <- weaveNewTokens index tok content 0 0 hi2 
          LoadedFileInfo (LSP.List hi2) tok newAspectPositions

loadFile :: LSP.Uri -> ServerM (LspM Config) ()
loadFile uri = 
    case LSP.uriToFilePath uri of 
      Nothing -> return ()
      Just path -> do
            (AbsolutePath absPath) <- liftIO $ absolute path
            newState <- runCommandM  $ do
                cmd_load' (unpack absPath) [] True Imp.TypeCheck $ \_ -> return ()
                tc <- TCM.getTC
                cm <- get
                index@(Index.LineIndex _ newLineSize) <- liftIO $ Index.fileToLineIndex (unpack absPath)
                tc <- TCM.getTC
                cm <- get
                fileContent <- liftIO $ readFile (unpack absPath)
                let file = CM.theCurrentFile cm
                
                case file of 
                  Nothing -> do
                      return empty
                  Just content -> do
                      let persistentState = TCM.stPersistentState tc
                      let modName = CM.currentFileModule content
                      let moduleInfo = Map.lookup modName (TCM.stDecodedModules persistentState)
                      case moduleInfo of
                          Nothing -> do
                              return empty
                          Just content -> do
                              --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                              let highlight = TCM.iHighlighting $ TCM.miInterface content
                              let listTokens = P.toList highlight
                              let aspectPositions = H.getAspectPositions 0 0 index listTokens
                              let semTokensContet = H.convertRangeTokens 0 0 H.defaultTokenMap aspectPositions 
                              let tokenMap = H.getTokenMap index H.defaultTokenMap (Array.listArray 
                                        (0, length fileContent) fileContent ) 0 0 aspectPositions
                              let tokenString = H.writeTokensResult semTokensContet
                              return (LoadedFileInfo semTokensContet tokenMap aspectPositions)
            case newState of
                Left _ -> return ()
                Right state -> do
                    c <- LSP.getConfig
                    LSP.setConfig (c { loadedFiles = Map.insert (unpack absPath) state (loadedFiles c)})

--------------------------------------------------------------------------------
-- Helper functions for converting stuff to SemanticTokenAbsolute


fromHighlightingInfo :: IR.HighlightingInfo -> LSP.SemanticTokenAbsolute
fromHighlightingInfo (IR.HighlightingInfo start end aspects isTokenBased note defSrc)
  = LSP.SemanticTokenAbsolute 1 1 3 LSP.SttKeyword []



-- HighlightingInfo
--       Int -- starting offset
--       Int -- ending offset
--       [String] -- list of names of aspects
--       Bool -- is token based?
--       String -- note
--       (Maybe (FilePath, Int)) -- the defining module of the token and its position in that module

-- toToken
--   :: Ranged a
--   => J.SemanticTokenTypes
--   -> [J.SemanticTokenModifiers]
--   -> a
--   -> [J.SemanticTokenAbsolute]
-- toToken types modifiers x =
--   let range = rangeOf x
--   in  [ J.SemanticTokenAbsolute (posLine (rangeStart range) - 1)
--                                 (posCol (rangeStart range) - 1)
--                                 (rangeSpan range)
--                                 types
--                                 modifiers
--       ]
