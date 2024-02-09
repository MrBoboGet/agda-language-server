{-# LANGUAGE CPP #-} module Server.Handler where

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
import qualified Agda.TypeChecking.Pretty as TCM
import qualified Agda.Interaction.Highlighting.Generate as G
import Agda.Utils.List            ( caseList, last1 )

import qualified Server.Highlight as H
import qualified Agda.Interaction.Base as CM
import qualified Agda.Interaction.Highlighting.Precise as P
import qualified Agda.Interaction.Highlighting.Range as A

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
                    let textLines = T.lines text
                    (AbsolutePath fp) <- liftIO $ absolute path
                    let pathString = unpack fp
                    if Map.member pathString (loadedFiles c) then do
                           let (Just file) = Map.lookup pathString (loadedFiles c)
                           let fileInfo@(LoadedFileInfo hi tok asp) = file
                           let newFile@(LoadedFileInfo nHi _ _) = updateLoadedFile fileInfo changes
                           liftIO $ writeFile "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/lineDiffs.txt" 
                                    (show (lineDiff (head changesList) , colDiff (head changesList)))
                           liftIO $ writeFile "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/sem1.txt" (H.writeTokensResult nHi)
                           liftIO $ writeFile "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/sem2.txt" (H.writeTokensResult hi)
                           let front@(LSP.TextDocumentContentChangeEvent (Just range) _ text) = head changesList
                           liftIO $ TIO.writeFile "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/text.txt" text
                           liftIO $ writeFile "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/range.txt" (show range)
                           LSPServer.setConfig (c {loadedFiles = Map.insert pathString newFile (loadedFiles c)  })
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



getTokenMap :: LSP.List LSP.UInt -> Text -> Map.Map String Int
getTokenMap _ _ = Map.empty

--updateHighlighting :: LSP.List LSP.UInt -> LSP.TextDocumentContentChangeEvent -> LSP.List LSP.UInt
--updateHighlighting _ _ = LSP.List []
pointIn :: LSP.UInt -> LSP.UInt -> LSP.Range -> Bool
pointIn line col range@(LSP.Range start end) | LSP._line start == LSP._line end = (line == LSP._line start) && (col >= LSP._character start) && (col < LSP._character end)
                                             | line == LSP._line start =  col >= LSP._character start
                                             | line > LSP._line start && line < LSP._line end = True
                                             | line == LSP._line end = col < LSP._character end
                                             | otherwise = False

rangeIn :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.Range -> Bool
rangeIn line col length range@(LSP.Range start end) = (pointIn line col range) || (pointIn line (col + length) range)
after :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.Range -> Bool
after line col length range@(LSP.Range start end) = not (rangeIn line col length range) && line >= LSP._line end && col >= LSP._character end

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
lineDiff e@(LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text) = LSP._line start - LSP._line end + lineCount text
lineDiff _ = 0
colDiff :: LSP.TextDocumentContentChangeEvent -> LSP.UInt
colDiff e@(LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ _) | LSP._line end == LSP._line start = LSP._character start - LSP._character end
                                                                                | otherwise = LSP._character end 
colDiff _ = 0

updateAspectMapImpl :: LSP.UInt -> LSP.UInt -> LSP.UInt -> LSP.UInt -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)] -> LSP.TextDocumentContentChangeEvent -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)]
updateAspectMapImpl eLine eCol tLine tCol (head@(dLine,dCol,cLength,a):tail) e@(LSP.TextDocumentContentChangeEvent (Just range@(LSP.Range start end)) _ _ ) = do
    let newLine = tLine + dLine
    let pnewCol = tCol + dCol
    let neCol = if dLine == 0 then eCol else 0
    let newCol = if dLine == 0 then pnewCol else dCol
    if rangeIn newLine newCol cLength range then 
        updateAspectMapImpl (dLine + eLine ) (dCol + neCol) newLine newCol tail e
    else
        if after newLine newCol cLength range then
            (dLine+(eLine+ lineDiff e)  , dCol+(neCol + colDiff e),cLength,a) : tail
            --(newLine ,newCol,cLength,a) : tail
        else
            head : updateAspectMapImpl 0 0 newLine newCol tail e
updateAspectMapImpl _ _ _ _ _ _ = []

updateAspectMap :: [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)] -> LSP.TextDocumentContentChangeEvent -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)]
updateAspectMap = updateAspectMapImpl 0 0 0 0

updateLoadedFile :: LoadedFileInfo -> LSP.List LSP.TextDocumentContentChangeEvent -> LoadedFileInfo
updateLoadedFile (LoadedFileInfo hi tok asp) (LSP.List updates) = do
          let newAspectPositions = foldl updateAspectMap asp updates
          let hi2 = H.convertRangeTokens 0 0 H.defaultTokenMap newAspectPositions
          LoadedFileInfo hi2 tok newAspectPositions

loadFile :: LSP.Uri -> ServerM (LspM Config) ()
loadFile uri = 
    case LSP.uriToFilePath uri of 
      Nothing -> return ()
      Just path -> do
            (AbsolutePath absPath) <- liftIO $ absolute path
            newState <- runCommandM  $ do
                cmd_load' (unpack absPath) [] True Imp.TypeCheck $ \_ -> return ()
                index <- liftIO $ Index.fileToLineIndex (unpack absPath)
                tc <- TCM.getTC
                cm <- get
                index <- liftIO $ Index.fileToLineIndex (unpack absPath)
                tc <- TCM.getTC
                cm <- get
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
                              let tokenMap = getTokenMap (LSP.List []) absPath 
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
