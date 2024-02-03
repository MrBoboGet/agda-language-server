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
import qualified Data.Text                     as Text
import           Language.LSP.Server            ( LspM )
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Types            as LSP
import qualified Language.LSP.VFS              as VFS
import           Monad
import           Options                        ( Config
                                                , Options(optRawAgdaOptions)
                                                )

import qualified Data.Strict as Strict 
import qualified Data.Map as Map 
import qualified Language.LSP.Types as LSPTypes 
import Data.List
import Agda.Interaction.Imports
import Agda.Syntax.TopLevelModuleName
import Agda.Utils.FileName
import Agda.Interaction.FindFile
import Agda.Interaction.Highlighting.FromAbstract
import qualified Agda.Interaction.Highlighting.Precise as P
import Agda.Utils.RangeMap
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Scope.Base
import qualified Agda.Syntax.Common.Aspect as Aspect
import Agda.Interaction.Highlighting.Range as A
import qualified Server.Index as Index
import qualified Language.LSP.Server as LSPServer


import qualified Agda.TypeChecking.Errors as TCM
import qualified Agda.TypeChecking.Monad  as TCM
import qualified Agda.TypeChecking.Pretty as TCM
import qualified Agda.Interaction.Highlighting.Generate as G
import qualified Agda.Utils.Impossible as IM
import qualified Data.IntMap as IntMap
import Agda.Utils.List            ( caseList, last1 )

import qualified Server.Highlight as H
import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Abstract as Abs
import qualified Agda.Interaction.Base as CM
import Data.Sequence

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

    

getAbstract :: FilePath -> ServerM (LspM Config) (Either String (TopLevelModuleName,Index.LineIndex  , TopLevelInfo))
getAbstract filepath = runCommandM $ do
  -- localStateCommandM: restore TC state afterwards, do we need this here?
  (n,i,t) <- localStateCommandM $ do
    fp <- liftIO $ absolute filepath
    s <- lift $ parseSource (SourceFile fp)
    index <- liftIO $ Index.fileToLineIndex filepath
    lift $ atTopLevel $ do 
            t <- concreteToAbstract_ (TopLevel fp (srcModuleName s) ([] :: [Declaration]) )
            return (srcModuleName s , index , t)
    --lift $ atTopLevel $ do 
    --        t <- concreteToAbstract_ (TopLevel fp (srcModuleName s) (modDecls (srcModule s)))
    --        return (srcModuleName s , index , t)
  return (n,i,t)

getHighlightFile :: FilePath -> ServerM (LspM Config) (Either String (Index.LineIndex , [(A.Range , P.Aspects)]))
getHighlightFile filepath = runCommandM $ do
         fp <- liftIO $ absolute filepath
         s <- lift $ parseSource (SourceFile fp)
         highlight <- lift $ atTopLevel $ do (getHighlightSource s)
         index <- liftIO $ Index.fileToLineIndex filepath
         return (index, highlight)

--getSemanticTokens :: 

--kindMap :: KindOfName -> NameKind
--kindMap ConName = Constructor Aspect.Inductive
--kindMap CoConName = Constructor Aspect.CoInductive
--kindMap FldName = Aspect.Record
--kindMap PatternSynName = Aspect.Function
--kindMap GeneralizeName = Aspect.Generalizable
--kindMap DisallowedGeneralizeName = Aspect.Generalizable
--kindMap MacroName = Aspect.Macro
--kindMap QuotableName = Aspect.Field
--kindMap DataName = Aspect.Record
--kindMap RecName = Aspect.Record
--kindMap FunName = Aspect.Function
--kindMap AxiomName = Aspect.Postulate
--kindMap PrimName = Aspect.Primitive
--kindMap OtherDefName = Aspect.Argument


nameLookup :: NameMap -> (QName -> Maybe P.NameKind)
nameLookup m x = case Map.lookup x m of 
                Nothing -> Nothing 
                Just a -> Just (P.kindOfNameToNameKind (qnameKind a))
getHighlightInfo :: TopLevelInfo -> TopLevelModuleName -> RangeMap P.Aspects
getHighlightInfo top@(TopLevelInfo decls scope) name = P.convert (runHighlighter name (nameLookup (_scopeInverseName scope)) decls)

getHighlightSource :: Source -> TCM.TCM [(A.Range , P.Aspects)]
getHighlightSource s = do
                         (CheckResult interface _ _ _) <- (typeCheckMain ScopeCheck s)
                         return (toList $ TCM.iHighlighting interface)

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


lineLength :: APosition.Interval ->  LSP.UInt
lineLength (APosition.Interval p1 p2) = fromInteger $ fromIntegral (APosition.posCol p2 - APosition.posCol p1)

convertTokenType :: Token -> LSP.SemanticTokenTypes 
convertTokenType (TokKeyword _ _) = LSP.SttNamespace
convertTokenType (TokId (_ , _)) = LSP.SttVariable
convertTokenType (TokQId _) = LSP.SttVariable
convertTokenType (TokLiteral _) = LSP.SttString
convertTokenType (TokSymbol _ _) = LSP.SttClass
convertTokenType (TokString _) = LSP.SttString
convertTokenType (TokTeX (_ , _)) = LSP.SttComment
convertTokenType (TokMarkup (_ , _)) = LSP.SttComment
convertTokenType (TokComment (_ , _)) = LSP.SttComment
convertTokenType TokDummy = LSP.SttNamespace
convertTokenType (TokEOF _) = LSP.SttNamespace

tokenInterval :: Token -> APosition.Interval
tokenInterval (TokKeyword _ i) = i
tokenInterval (TokId (i , _)) = i
tokenInterval (TokQId (head : tail)) = fst head
tokenInterval (TokQId []) = APosition.Interval   (APosition.Pn Strict.Nothing 0 0 0)  (APosition.Pn Strict.Nothing 0 0 0)
tokenInterval (TokLiteral _) = APosition.Interval   (APosition.Pn Strict.Nothing 0 0 0)  (APosition.Pn Strict.Nothing 0 0 0)
tokenInterval (TokSymbol _ i) = i
tokenInterval (TokString (i , _)) = i
tokenInterval (TokTeX (i , _)) = i
tokenInterval (TokMarkup (i , _)) = i
tokenInterval (TokComment (i , _)) = i
tokenInterval TokDummy = APosition.Interval   (APosition.Pn Strict.Nothing 0 0 0)  (APosition.Pn Strict.Nothing 0 0 0)
tokenInterval (TokEOF i) = i

tokenLength :: Token -> LSP.UInt
tokenLength t = lineLength $ tokenInterval t

tokenLineStart :: Token -> LSP.UInt
tokenLineStart t = (fromIntegral $ APosition.posLine (APosition.iStart $ tokenInterval t)) - fromIntegral 1
tokenColStart :: Token -> LSP.UInt
tokenColStart t = (fromIntegral $ APosition.posCol (APosition.iStart $ tokenInterval t)) - fromIntegral 1

convertTokens :: (LSP.SemanticTokenTypes -> LSP.UInt) -> [Token] -> LSP.Position -> LSP.List LSP.UInt
convertTokens f [] (LSP.Position l  r) = LSP.List []
convertTokens f (head : tail) (LSP.Position l  r) = LSP.List [ tokenLineStart head - l , tokenColStart head - r , 2 , f $ convertTokenType head , 0] <> convertTokens f tail (LSP.Position (tokenLineStart head) (tokenColStart head))


--defaultMap :: Map.Map;
--defaultMap = Map.fromList (LSPTypes.SemanticTokensLegend)

defaultTokenMap :: LSP.SemanticTokenTypes -> LSP.UInt
defaultTokenMap a = fromIntegral $ case (elemIndex a LSP.knownSemanticTokenTypes) of
                                        Nothing -> 0
                                        Just i -> i


--type ASTInfo = [(LSP.Position , Aspects)]
--convertHighlightInfo :: Index.LineIndex -> RangeMap Aspects -> ASTInfo
--convertHighlightInfo index rangeMap = do 
--                                        a <- toList rangeMap
--                                        b <- map (\((A.Range start end),content) -> ((LSP.Position (fromIntegral start) (fromIntegral end)) , content)) a
--                                        []

nameKindToSemantic :: P.NameKind -> LSP.SemanticTokenTypes
nameKindToSemantic (P.Constructor _) = LSP.SttMacro
nameKindToSemantic P.Datatype = LSP.SttClass
nameKindToSemantic P.Field = LSP.SttVariable
nameKindToSemantic P.Module = LSP.SttNamespace
nameKindToSemantic P.Postulate = LSP.SttMacro
nameKindToSemantic P.Primitive = LSP.SttClass
nameKindToSemantic P.Record = LSP.SttMacro
nameKindToSemantic P.Argument = LSP.SttVariable
nameKindToSemantic P.Macro = LSP.SttMacro
nameKindToSemantic _ = LSP.SttFunction

aspectToSemantic :: P.Aspect -> LSP.SemanticTokenTypes
aspectToSemantic P.Comment = LSP.SttComment
aspectToSemantic P.Keyword = LSP.SttKeyword
aspectToSemantic P.String = LSP.SttString
aspectToSemantic P.Number = LSP.SttNumber
aspectToSemantic P.Hole = LSP.SttString
aspectToSemantic P.Symbol = LSP.SttVariable
aspectToSemantic P.PrimitiveType = LSP.SttClass
aspectToSemantic (P.Name Nothing _) = LSP.SttClass
aspectToSemantic (P.Name (Just kind) _) = nameKindToSemantic kind
aspectToSemantic P.Pragma = LSP.SttComment
aspectToSemantic P.Background = LSP.SttComment
aspectToSemantic P.Markup = LSP.SttComment

convertRangeTokens :: Int -> Int -> Index.LineIndex -> (LSP.SemanticTokenTypes -> LSP.UInt) -> [(A.Range , P.Aspects)] -> LSP.List LSP.UInt
convertRangeTokens prevLine prevCol index map ( (A.Range start end , a) : tail) = case P.aspect a of 
    Nothing -> convertRangeTokens prevLine prevCol index map tail
    Just asp -> do 
                  let (LSP.Position line col) = Index.offsetToPosition index start
                  let (LSP.Position endLine endCol) = Index.offsetToPosition index end
                  let deltaCol = if line == fromIntegral prevLine  then  col - (fromIntegral prevCol) else col
                  let length = if endCol > col  then  endCol - col else (if line /= endLine then 100+endLine else 1000+line)
                  (LSP.List 
                    [ line - (fromIntegral prevLine) , deltaCol , length , map $ aspectToSemantic asp, 0]) 
                    <> convertRangeTokens (fromIntegral line) (fromIntegral col) index map tail
convertRangeTokens line col index map [] = LSP.List []

getDeclarationHighlight :: Abs.Declaration -> ServerM (LspM Config) (Either String [(APosition.Range,P.Aspects)])
getDeclarationHighlight decl = runCommandM $  do
    result <- lift $ atTopLevel $ H.generateSyntaxInfo decl False
    return result

getTopHighlightInfo :: [Abs.Declaration] -> ServerM (LspM Config) [(APosition.Range,P.Aspects)]
getTopHighlightInfo [] = do return []
getTopHighlightInfo (head : tail) = do 
                                declHighlight <- getDeclarationHighlight head
                                rest <- getTopHighlightInfo tail
                                case declHighlight of 
                                    Left _ -> return rest
                                    Right headTokens -> return (headTokens <> rest)


getWriteContent :: [(A.Range , P.Aspects)] -> String
getWriteContent input = foldl (<>) "" (map (\((A.Range x y),_) -> (show x) <> " " <> (show y) <> "\n") input)
writeResult :: String -> String -> IO ()
writeResult file input = do
                writeFile file input
                return ()

writeIndexResult :: Index.LineIndex -> [(A.Range , P.Aspects)] -> String
writeIndexResult index aspects = foldl (<>) "" (map (\((LSP.Position x y),(LSP.Position x2 y2)) -> "(" <> (show x) <> "," <> (show y)<>") ("<> (show x2) <> "," <> (show y2) <> ")\n")  ((map (\((A.Range x y) , _) -> ((Index.offsetToPosition index x),(Index.offsetToPosition index y))))   aspects))

writeTokensResult :: LSP.List LSP.UInt -> String
writeTokensResult (LSP.List (x1:x2:x3:x4:x5:tail)) = (foldl (\x y -> x <> " " <> (show y)) "" [x1,x2,x3,x4,x5]) <> "\n" <> 
    (writeTokensResult (LSP.List tail))
writeTokensResult _ = ""


onHighlight :: LSP.Uri -> ServerM (LspM Config) (Maybe LSP.SemanticTokens)
onHighlight ur =
  case LSP.uriToFilePath ur of 
    Nothing -> return Nothing
    Just path -> do 
      (AbsolutePath fp) <- liftIO $ absolute path
      result <- runCommandM $ do  
          cmd_load' (unpack fp) [] True Imp.TypeCheck $ \_ -> return ()
          index <- liftIO $ Index.fileToLineIndex (unpack fp)
          tc <- TCM.getTC
          cm <- get
          let file = CM.theCurrentFile cm
          
          (stringToPrint, indexString , tokenResult,tokenReturn) <- case file of 
            Nothing -> do
                --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                return ("","","",(Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttVariable ,0]) )))
            Just content -> do
                let persistentState = TCM.stPersistentState tc
                let modName = CM.currentFileModule content
                let moduleInfo = Map.lookup modName (TCM.stDecodedModules persistentState)
                case moduleInfo of
                    Nothing -> do
                        --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                        return ("","","",(Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttClass ,0]) )))
                    Just content -> do
                        --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                        let highlight = TCM.iHighlighting $ TCM.miInterface content
                        let listTokens = toList highlight
                        let semTokensContet = convertRangeTokens 0 0 index defaultTokenMap listTokens
                        let semTokens = (LSP.SemanticTokens Nothing semTokensContet)
                        return (getWriteContent listTokens, writeIndexResult index listTokens, writeTokensResult semTokensContet, (Just semTokens ))
          liftIO (writeResult "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/offset.txt" stringToPrint)
          liftIO (writeResult "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/index.txt" indexString)
          liftIO (writeResult "C:/Users/emanu/Desktop/Program/Haskell/agda-language-server/sem.txt" tokenResult)
          return tokenReturn
      case result of 
           Left s -> do
                  LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack s))
                  return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttMacro ,0]) ))
           Right tokens -> do
                  return tokens
--return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttClass ,0]) ))
      ----return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttClass ,0]) ))
      --case result of
      --  Nothing   -> return Nothing
      --  Just file -> do
      --    let source      = VFS.virtualFileText file
      --    --return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttClass ,0]) ))
      --    mAbstract <- getAbstract path 
      --    case mAbstract of
      --      Left s -> do
      --                  LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack s))
      --                  return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,defaultTokenMap LSP.SttClass ,0]) ))
      --      Right (n,index,ast) -> do
      --            return (Just (LSP.SemanticTokens Nothing (convertRangeTokens 0 0 index defaultTokenMap (toList (getHighlightInfo ast n)))))
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
