module Server.Highlight where
import qualified Agda.Interaction.Highlighting.Precise as P
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Types            as LSP
import qualified Server.Index as Index
import Agda.Interaction.Highlighting.Range as A
import Data.List

defaultTokenMap :: LSP.SemanticTokenTypes -> LSP.UInt
defaultTokenMap a = fromIntegral $ case (elemIndex a LSP.knownSemanticTokenTypes) of
                                        Nothing -> 0
                                        Just i -> i


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

