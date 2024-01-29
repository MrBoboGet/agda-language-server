module Server.Index where

import Data.Array
import qualified Language.LSP.Types as LSP
import Agda.Interaction.Highlighting.Range as A


type LineIndex = (Array Int Int)

newLines :: Int -> String -> [Int]
newLines offset [] = []
newLines offset ('n':tail) = offset : newLines (offset + 1) tail
newLines offset (x:tail) = newLines (offset + 1) tail

fileToLindeIndex :: FilePath -> IO LineIndex
fileToLindeIndex p = do 
                        text <- readFile p
                        let lines = 0 : newLines 0 text
                        let result = listArray (0,length lines) lines
                        return result

offsetToPosition :: LineIndex -> Int -> LSP.Position
offsetToPosition index offset = let line = max (binarySearch (<) index offset-1) 0 in
                                LSP.Position (fromIntegral line) (fromIntegral (offset - fromIntegral (index ! line)))
positionToOffset :: LineIndex -> LSP.Position -> Int
positionToOffset index (LSP.Position line col) = (index ! (fromIntegral line)) + fromIntegral col

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
