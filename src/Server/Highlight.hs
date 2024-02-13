module Server.Highlight where
import qualified Agda.Interaction.Highlighting.Precise as P
import qualified Language.LSP.Server           as LSP
import qualified Language.LSP.Types            as LSP
import Agda.Interaction.Highlighting.Range as A
import Data.List
import Data.Array
import qualified Data.Map as Map
import qualified Server.Index as Index

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


getAspectPositions :: Int -> Int -> Index.LineIndex -> [(A.Range , P.Aspects)] -> [(LSP.UInt,LSP.UInt,LSP.UInt,P.Aspects)]
getAspectPositions prevLine prevCol index [] = []
getAspectPositions prevLine prevCol index ( (A.Range start end , a) : tail) = 
    do
      let (LSP.Position line col) = Index.offsetToPosition index start
      let (LSP.Position endLine endCol) = Index.offsetToPosition index end
      let deltaCol = if line == fromIntegral prevLine  then  col - (fromIntegral prevCol) else col
      let length = if endCol > col  then  endCol - col else (if line /= endLine then 100+endLine else 1000+line)
      (line - (fromIntegral prevLine) , deltaCol , length , a) : (getAspectPositions (fromIntegral line) (fromIntegral col) index tail)

convertRangeTokens :: LSP.UInt -> LSP.UInt -> (LSP.SemanticTokenTypes -> LSP.UInt) -> [(LSP.UInt , LSP.UInt, LSP.UInt ,P.Aspects)] -> LSP.List LSP.UInt
convertRangeTokens eLine eCol map ( (dLine,dCol,length , a) : tail) = 
    let newELine = eLine + dLine in 
    let newECol = if dLine == 0 then eCol + dCol else dCol in 
    case P.aspect a of 
    Nothing -> convertRangeTokens newELine newECol map tail
    Just asp -> do 
                  if (asp == P.Symbol) then do
                      convertRangeTokens newELine newECol map tail
                  else
                    do
                      (LSP.List 
                        [ dLine + eLine , dCol + (if dLine == 0 then eCol else 0), length , map $ aspectToSemantic asp, 0]) 
                        <> convertRangeTokens 0 0 map tail
convertRangeTokens _ _ map [] = LSP.List []


getString :: Index.LineIndex -> Array Int Char -> LSP.UInt -> LSP.UInt -> LSP.UInt -> String
getString index content line col length = 
    do 
        if length == 0 then "" else do
            let offset = Index.positionToOffset index (LSP.Position line col)
            if offset >= snd (bounds content) then "" else do
                if (content ! offset) == '\n' || (content ! offset) == '\r' then "" else  
                    (content ! offset) : getString index content line (col + 1) (length - 1)

getTokenMap :: Index.LineIndex -> (LSP.SemanticTokenTypes -> LSP.UInt) ->  Array Int Char -> 
            LSP.UInt -> LSP.UInt -> [(LSP.UInt ,LSP.UInt,LSP.UInt,P.Aspects)] -> Map.Map String LSP.UInt
getTokenMap index tmap content tLine tCol ((dLine,dCol,cLength,a):tail) = do
    let newCol = if dLine == 0 then dCol+tCol else dCol 
    let newLine = tLine+dLine 
    case P.aspect a of
        Nothing -> getTokenMap index tmap content newLine newCol tail
        (Just asp) -> 
            do 
                if asp == P.Symbol then getTokenMap index tmap content newLine newCol tail else do
                  let newToken = getString index content newLine newCol cLength 
                  let recMap = getTokenMap index tmap content newLine newCol tail 
                  if Map.member newToken recMap then
                      recMap
                  else 
                      Map.insert newToken (tmap (aspectToSemantic asp)) recMap
getTokenMap _ _ _ _ _ _ = empty


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

getAspectListString :: [(LSP.UInt,LSP.UInt,LSP.UInt,a)] -> String
getAspectListString l = foldl (\left (x1,x2,x3,_) -> left <>  (show (x1,x2,x3) <> "\n")) "" l
