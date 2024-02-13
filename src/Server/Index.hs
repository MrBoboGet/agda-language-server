module Server.Index where

import Data.Array
import qualified Language.LSP.Types as LSP
import Agda.Interaction.Highlighting.Range as A


data LineIndex = LineIndex (Array Int Int) Int 

instance Show LineIndex where
    show (LineIndex a l) = (show a) <> "\nNewLineType " <> (show l)
empty :: LineIndex
empty = LineIndex (array (0,0) []) 2

newLines :: Int -> String -> ([Int],Int)
newLines offset [] = ([],1)
newLines offset ('\n':tail) = do
        let head =  offset 
        let (recTail,size) = newLines (offset + 1) tail
        (head:recTail , size)
newLines offset ('\r':tail) = do 
    let (result,_) = newLines (offset + 1) tail
    (result,2)
newLines offset (x:tail) = newLines (offset + 1) tail

stringToLineIndex :: String -> LineIndex
stringToLineIndex text = do
    let (oLines,newLineSize) = newLines 0 text
    let lines = 0:oLines
    let result = listArray (0,length lines) lines
    (LineIndex result newLineSize)

fileToLineIndex :: FilePath -> IO LineIndex
fileToLineIndex p = do 
                        text <- readFile p
                        return (stringToLineIndex text)

offsetToPosition :: LineIndex -> Int -> LSP.Position
offsetToPosition index 1 = LSP.Position 0 0
offsetToPosition (LineIndex index newlineSize) offset = let line = max (binarySearch (<) index offset-1) 0 in
                                let lineOffset = fromIntegral (index ! line) in
                                if line == 0 then (LSP.Position 0 (fromIntegral  (offset - 1))) else 
                                if line > 0 && (offset-1 == lineOffset) then
                                    (LSP.Position (fromIntegral (line -1)) (fromIntegral (lineOffset - (index ! (line -1)))))
                                else
                                    LSP.Position (fromIntegral line) (fromIntegral (offset - (fromIntegral (index ! line) + 2)))
positionToOffset :: LineIndex -> LSP.Position -> Int
positionToOffset (LineIndex index newlineSize) (LSP.Position line col) = ((index ! (fromIntegral line)) + fromIntegral col)+1

binarySearchImpl :: Int -> Int -> (a -> b -> Bool) -> Array Int a -> b -> Int
binarySearchImpl lower upper less array target = let index = div (lower + upper) 2
                                                     currentElem = (array ! index) in
                                                 if lower >= upper then 
                                                    upper
                                                 else
                                                     if less currentElem target then
                                                       binarySearchImpl (index + 1) upper less array target
                                                     else
                                                       binarySearchImpl lower index less array target

binarySearch :: (a -> b -> Bool) -> Array Int a -> b -> Int
binarySearch less array = binarySearchImpl 0 (length array-1)  less array 


rangeToLSPRange :: LineIndex -> A.Range -> LSP.Range
rangeToLSPRange index (A.Range f t) = LSP.Range (offsetToPosition index f) (offsetToPosition index t)

less :: LSP.Range -> LSP.Position -> Bool
less (LSP.Range (LSP.Position l1 c1)  (LSP.Position l2 c2)) (LSP.Position line col) = 
            (line >= l1 && line <= l2) && (col >= c1 && col <= c2)
rangeLess :: A.Range -> Int  -> Bool
rangeLess (A.Range start end) i = (i >= start && i < end)
