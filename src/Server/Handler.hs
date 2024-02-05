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
import qualified Language.LSP.VFS              as VFS
import           Monad
import           Options                        ( Config
                                                , Options(optRawAgdaOptions)
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
          
          tokenReturn <- case file of 
            Nothing -> do
                --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,H.defaultTokenMap LSP.SttVariable ,0]) ))
            Just content -> do
                let persistentState = TCM.stPersistentState tc
                let modName = CM.currentFileModule content
                let moduleInfo = Map.lookup modName (TCM.stDecodedModules persistentState)
                case moduleInfo of
                    Nothing -> do
                        --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                        return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,H.defaultTokenMap LSP.SttClass ,0])))
                    Just content -> do
                        --LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack "no current file :((("))
                        let highlight = TCM.iHighlighting $ TCM.miInterface content
                        let listTokens = P.toList highlight
                        let semTokensContet = H.convertRangeTokens 0 0 index H.defaultTokenMap listTokens
                        let semTokens = (LSP.SemanticTokens Nothing semTokensContet)
                        return  (Just semTokens)
          return tokenReturn
      case result of 
           Left s -> do
                  LSPServer.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtError (pack s))
                  return (Just (LSP.SemanticTokens Nothing (LSP.List [0,0,5,H.defaultTokenMap LSP.SttMacro ,0]) ))
           Right tokens -> do
                  return tokens
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
