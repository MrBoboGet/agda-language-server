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

offsetsToMultilineString :: Array.Array Int Char -> Int -> Int -> String
offsetsToMultilineString content start end | fromIntegral start >= snd (Array.bounds content) = ""
                                  | start == end = ""
                                  | otherwise = (content ! start):offsetsToMultilineString content (start + 1) end

parseTokens :: Index.LineIndex -> Array.Array Int Char -> LSP.UInt -> LSP.UInt -> PM [Token]
parseTokens index content startOffset endOffset = do
    let startPosition = (Index.offsetToPosition index (fromIntegral startOffset))
    let initPos = (Pn {srcFile = SM.Nothing , posPos = fromIntegral startOffset , posLine = fromIntegral (LSP._line startPosition) , 
            posCol = fromIntegral (LSP._character startPosition) })
    (result, a) <- parsePosString tokensParser initPos (offsetsToMultilineString content (fromIntegral (startOffset - 1)) (fromIntegral (endOffset - 1) ))
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

parseTokenIntervals :: Index.LineIndex -> Array.Array Int Char -> LSP.UInt -> LSP.UInt -> PM [LSP.Range]
parseTokenIntervals index content startOffset endOffset = do
    tokens <-  parseTokens index content startOffset endOffset
    let newTokens = filter (not . isSymbol) tokens
    let intervals = concatMap tokenToIntervalls newTokens
    let returnValue = map (intervallToRange index) intervals
    return returnValue
