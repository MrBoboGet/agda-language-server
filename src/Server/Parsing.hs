module Server.Parsing where
import           Agda.Interaction.BasicOps      ( atTopLevel
                                                , typeInCurrent
                                                )
import Agda.Syntax.Parser                   as P
import qualified Server.Index as Index
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Types            as LSP
import Agda.Syntax.Parser.Tokens 
import Agda.Syntax.Position
import qualified Data.Array as Array
import Data.Array ((!))
import Data.Strict.Maybe as SM
import Data.Foldable

import Control.Monad.IO.Class

offsetsToString :: Array.Array Int Char -> Int -> Int -> String
offsetsToString content start end | fromIntegral start >= snd (Array.bounds content) = ""
                                  | start == end = ""
                                  | otherwise = if (content ! start) == '\r' || (content ! start) == '\n' then "" else (content ! start):offsetsToString content (start + 1) end

parseTokens :: Index.LineIndex -> Array.Array Int Char -> LSP.UInt -> LSP.UInt -> LSP.UInt -> PM [Token]
parseTokens index content line col lastCol = do
    let characterOffset = (Index.positionToOffset index  (LSP.Position line col)) + 1
    let initPos = (Pn {srcFile = SM.Nothing , posPos = fromIntegral characterOffset , posLine = fromIntegral line , posCol = fromIntegral col})
    (result, a) <- parsePosString tokensParser initPos (offsetsToString content (characterOffset - 1) (characterOffset - 1 + (fromIntegral  (lastCol - col)) ))
    return result


tokenToIntervalls :: Token -> [IntervalWithoutFile]
tokenToIntervalls token = (\(Range _ ints) -> toList ints) (getRange token)

aPosToLSPPos :: Index.LineIndex -> Position' a -> LSP.Position
aPosToLSPPos index pos = Index.offsetToPosition index (fromIntegral (posPos pos))

intervallToRange :: Index.LineIndex -> IntervalWithoutFile -> LSP.Range
intervallToRange index interval = (LSP.Range (aPosToLSPPos index (iStart interval)) (aPosToLSPPos index (iEnd interval)))

isSymbol :: Token -> Bool
isSymbol (TokSymbol _ _) = True
isSymbol _ = False

parseTokenIntervals :: Index.LineIndex -> Array.Array Int Char -> LSP.UInt -> LSP.UInt -> LSP.UInt -> PM [LSP.Range]
parseTokenIntervals index content line col lastCol = do
    tokens <-  parseTokens index content line col lastCol
    let newTokens = filter (not . isSymbol) tokens
    let intervals = foldl (<>) [] (map tokenToIntervalls newTokens)
    let returnValue = map (intervallToRange index) intervals
    return returnValue
