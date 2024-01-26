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
import           Agda.Interaction.Highlighting.Precise
                                                ( HighlightingInfo )
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
                                                , parse
                                                )
import           Agda.Syntax.Parser.Tokens      ( Token(..))
import           Agda.Syntax.Translation.ConcreteToAbstract
                                                ( concreteToAbstract_ )
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
tokenLineStart t = fromIntegral $ APosition.posLine (APosition.iStart $ tokenInterval t)
tokenColStart :: Token -> LSP.UInt
tokenColStart t = fromIntegral $ APosition.posCol (APosition.iStart $ tokenInterval t)

convertTokens :: (LSP.SemanticTokenTypes -> LSP.UInt) -> [Token] -> LSP.Position -> LSP.List LSP.UInt
convertTokens f [] (LSP.Position l  r) = LSP.List []
convertTokens f (head : tail) (LSP.Position l  r) = LSP.List [ tokenLineStart head - l , tokenColStart head - r , 2 , f $ convertTokenType head , 0] <> convertTokens f tail (LSP.Position (tokenLineStart head) (tokenColStart head))


--defaultMap :: Map.Map;
--defaultMap = Map.fromList (LSPTypes.SemanticTokensLegend)

defaultTokenMap :: LSP.SemanticTokenTypes -> LSP.UInt
defaultTokenMap a = fromIntegral $ case (elemIndex a LSP.knownSemanticTokenTypes) of
                                        Nothing -> 0
                                        Just i -> i


onHighlight :: LSP.Uri -> ServerM (LspM Config) (Maybe LSP.SemanticTokens)
onHighlight uri = do
  result <- LSP.getVirtualFile (LSP.toNormalizedUri uri)
  case result of
    Nothing   -> return Nothing
    Just file -> do
      let source      = VFS.virtualFileText file
      let offsetTable = makeToOffset source
      mtokens <- Parser.getTokens uri source
      case mtokens of
        Nothing             -> return Nothing
        Just tokens -> do
          case LSP.uriToFilePath uri of
            Nothing       -> return Nothing
            Just filepath -> do
              return (Just (LSP.SemanticTokens Nothing (convertTokens  (const 1) tokens (LSP.Position 0 0) )))
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
