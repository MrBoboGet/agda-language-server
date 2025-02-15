{-# LANGUAGE DeriveGeneric #-}
module Options
  ( Options(..)
  , getOptionsFromArgv
  , usageMessage
  , Config(..)
  , initConfig
  , empty
  , LoadedFileInfo(..)
  ) where
import           Data.Aeson.Types        hiding ( Options
                                                , defaultOptions
                                                )
import           GHC.Generics                   ( Generic )
import           System.Console.GetOpt
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )
import qualified Data.Map as Map
import qualified Language.LSP.Types as LSP
import qualified Agda.Interaction.Highlighting.Precise as P
import qualified Agda.Interaction.Base as IB
import qualified Server.Index as Index
import qualified Agda.TypeChecking.Monad.Base as TC

getOptionsFromArgv :: IO Options
getOptionsFromArgv = do
  -- extract options for Agda from ARGV 
  (argvForALS, argvForAgda) <- extractAgdaOpts <$> getArgs
  -- parse options for ALS 
  (opts      , _          ) <- parseOpts argvForALS
  -- save options for Agda back
  return $ opts { optRawAgdaOptions = argvForAgda }

usageMessage :: String
usageMessage = usageInfo usage options ++ usageAboutAgdaOptions

--------------------------------------------------------------------------------

-- | Command-line arguments
data Options = Options
  { optViaTCP         :: Maybe Int
  , optRawAgdaOptions :: [String]
  , optHelp           :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options { optViaTCP = Nothing, optRawAgdaOptions = [], optHelp = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optHelp = True }))
           "print this help message"
  , Option
    ['p']
    ["port"]
    (OptArg
      (\port opts -> case port of
        Just n  -> opts { optViaTCP = readMaybe n }
        Nothing -> opts { optViaTCP = Just 4096 }
      )
      "PORT"
    )
    "talk with the editor via TCP port (4096 as default)"
  ]

usage :: String
usage = "Agda Language Server v0.0.3.0 \nUsage: als [Options...]\n"

usageAboutAgdaOptions :: String
usageAboutAgdaOptions =
  "\n\
      \  +AGDA [Options for Agda ...] -AGDA\n\
      \    To pass command line options to Agda, put them in between '+AGDA' and '-AGDA'\n\
      \    For example:\n\
      \      als -p=3000 +AGDA --cubical -AGDA\n\
      \    If you are using agda-mode on VS Code, put them in the Settings at:\n\
      \      agdaMode.connection.commandLineOptions\n\
      \"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo usage options


-- | Removes RTS options from a list of options (stolen from Agda)
stripRTS :: [String] -> [String]
stripRTS []               = []
stripRTS ("--RTS" : argv) = argv
stripRTS (arg : argv)
  | is "+RTS" arg = stripRTS $ drop 1 $ dropWhile (not . is "-RTS") argv
  | otherwise     = arg : stripRTS argv
  where is x arg = [x] == take 1 (words arg)

-- | Extract Agda options (+AGDA ... -AGDA) from a list of options
extractAgdaOpts :: [String] -> ([String], [String])
extractAgdaOpts argv =
  let (before , argv') = break (== "+AGDA") argv
      (forAgda, after) = break (== "-AGDA") argv'
      forALS           = before ++ dropWhile (== "-AGDA") after
      forAgda'         = dropWhile (== "+AGDA") forAgda
  in  (forALS, forAgda')

--------------------------------------------------------------------------------

data LoadedFileInfo = LoadedFileInfo { hi :: (LSP.List LSP.UInt) , tok :: (Map.Map String LSP.UInt) , asp :: [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)]}
  deriving (Eq, Show, Generic)
data Config = Config { configRawAgdaOptions :: [String] , loadedFiles :: Map.Map String LoadedFileInfo , 
    persistentTCState :: (Maybe TC.PreScopeState)}
  deriving (Generic)

empty :: LoadedFileInfo
empty = LoadedFileInfo (LSP.List []) Map.empty [] 

makeConf :: [String] -> Config
makeConf strings = Config { configRawAgdaOptions = strings, loadedFiles = Map.empty, persistentTCState = Nothing}

instance FromJSON Config where
  parseJSON (Object v) = makeConf <$> v .: "commandLineOptions"
  -- We do not expect a non-Object value here.
  -- We could use empty to fail, but typeMismatch
  -- gives a much more informative error message.
  parseJSON invalid =
    prependFailure "parsing Config failed, " (typeMismatch "Object" invalid)

initConfig :: Config
initConfig = makeConf []
