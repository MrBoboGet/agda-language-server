{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Server.Highlight where

import Prelude hiding (null)

import Control.Monad

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Maybe
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.IntMap as IntMap
import Data.Semigroup (Semigroup(..))
import Data.Sequence (Seq)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text

import Agda.Interaction.Response
  ( RemoveTokenBasedHighlighting( KeepHighlighting ) )
import Agda.Interaction.Highlighting.Precise as H
import Agda.Interaction.Highlighting.Range
  (rToR, rangeToRange, overlappings, Ranges)
import Agda.Interaction.Highlighting.FromAbstract

import qualified Agda.TypeChecking.Errors as TCM
import Agda.TypeChecking.MetaVars (isBlockedTerm, hasTwinMeta)
import Agda.TypeChecking.Monad
  hiding (ModuleInfo, MetaInfo, Primitive, Constructor, Record, Function, Datatype)
import qualified Agda.TypeChecking.Monad  as TCM
import qualified Agda.TypeChecking.Monad.Base.Warning as W
import qualified Agda.TypeChecking.Pretty as TCM
import Agda.TypeChecking.Positivity.Occurrence
import Agda.TypeChecking.Warnings ( raiseWarningsOnUsage, runPM )

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Concrete.Definitions as W ( DeclarationWarning(..), DeclarationWarning'(..) )
import qualified Agda.Syntax.Concrete.Name as C
import qualified Agda.Syntax.Internal as I
import qualified Agda.Syntax.Literal as L
import qualified Agda.Syntax.Parser as Pa
import qualified Agda.Syntax.Parser.Tokens as T
import qualified Agda.Syntax.Position as P
import Agda.Syntax.Position
  (RangeFile, Range, HasRange, getRange, noRange)
import Agda.Syntax.TopLevelModuleName

import Agda.Syntax.Scope.Base     ( WithKind(..) )
import Agda.Syntax.Abstract.Views ( KName, declaredNames )

import Agda.Utils.FileName
import Agda.Utils.List            ( caseList, last1 )
import Agda.Utils.List2           ( List2 )
import qualified Agda.Utils.List2 as List2
import Agda.Utils.Maybe
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.Null
import Agda.Syntax.Common.Pretty
import Agda.Utils.Singleton

import Agda.Utils.Impossible

import qualified Agda.Interaction.Highlighting.Generate as G

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import qualified Agda.Syntax.Common.Aspect as ASP

generateSyntaxInfo
  :: A.Declaration
       -- ^ Declaration to highlight.
  -> Bool
       -- ^ Update the state?
  -> TCM [(Range , ASP.Aspects)]
generateSyntaxInfo decl _ | null $ getRange decl = return []
generateSyntaxInfo decl updateState = let hlLevel = G.Partial  in do
  top <- fromMaybe __IMPOSSIBLE__ <$> currentTopLevelModule
  ignoreAbstractMode $ do
    kinds <- nameKinds hlLevel decl

    -- After the code has been type checked more information may be
    -- available for overloaded constructors, and
    -- @generateConstructorInfo@ takes advantage of this information.
    -- Note, however, that highlighting for overloaded constructors is
    -- included also in @nameInfo@.
    constructorInfo <- case hlLevel of
      G.Full{} -> generateConstructorInfo top kinds decl
      _      -> return mempty

    -- Main source of scope-checker generated highlighting:
    let nameInfo = runHighlighter top kinds decl
    -- Highlighting from the lexer and Happy parser:
    (curTokens, otherTokens) <-
      insideAndOutside (rangeToRange (getRange decl)) <$> useTC stTokens

    -- @constructorInfo@ needs
    -- to be placed before @nameInfo@ since, when typechecking is done,
    -- constructors are included in both lists. Finally the token
    -- information is placed last since token highlighting is more
    -- crude than the others.
    let syntaxInfo = convert (constructorInfo <> nameInfo)
                       <>
                     curTokens

    when updateState $ do
      stSyntaxInfo `modifyTCLens` mappend syntaxInfo
      stTokens     `setTCLens`    otherTokens

    return []
generateConstructorInfo
  :: TopLevelModuleName
     -- ^ The module to highlight.
  -> NameKinds
  -> A.Declaration
  -> TCM HighlightingInfoBuilder
generateConstructorInfo top kinds decl = do

  -- Get boundaries of current declaration.
  -- @noRange@ should be impossible, but in case of @noRange@
  -- it makes sense to return mempty.
  caseList (P.rangeIntervals $ getRange decl)
           (return mempty) $ \ i is -> do
    let start = fromIntegral $ P.posPos $ P.iStart i
        end   = fromIntegral $ P.posPos $ P.iEnd $ last1 i is

    -- Get all disambiguated names that fall within the range of decl.
    m0 <- useTC stDisambiguatedNames
    let (_, m1) = IntMap.split (pred start) m0
        (m2, _) = IntMap.split end m1
        constrs = IntMap.elems m2

    -- Return suitable syntax highlighting information.
    return $ foldMap (runHighlighter top kinds) constrs



nameKinds :: G.Level
             -- ^ This should only be @'Full'@ if
             -- type-checking completed successfully (without any
             -- errors).
          -> A.Declaration
          -> TCM NameKinds
nameKinds hlLevel decl = do
  imported <- useTC $ stImports . sigDefinitions
  local    <- case hlLevel of
    G.Full{} -> useTC $ stSignature . sigDefinitions
    _      -> return HMap.empty
  impPatSyns <- useTC stPatternSynImports
  locPatSyns <- case hlLevel of
    G.Full{} -> useTC stPatternSyns
    _      -> return empty
      -- Traverses the syntax tree and constructs a map from qualified
      -- names to name kinds. TODO: Handle open public.
  let syntax :: NameKindMap
      syntax = runBuilder (declaredNames decl :: NameKindBuilder) HMap.empty
  return $ \ n -> unionsMaybeWith mergeNameKind
    [ defnToKind . theDef <$> HMap.lookup n local
    , con <$ Map.lookup n locPatSyns
    , defnToKind . theDef <$> HMap.lookup n imported
    , con <$ Map.lookup n impPatSyns
    , HMap.lookup n syntax
    ]
  where
  defnToKind :: TCM.Defn -> NameKind
  defnToKind   TCM.Axiom{}                           = Postulate
  defnToKind   TCM.DataOrRecSig{}                    = Postulate
  defnToKind   TCM.GeneralizableVar{}                = Generalizable
  defnToKind d@TCM.Function{} | isProperProjection d = Field
                            | otherwise            = Function
  defnToKind   TCM.Datatype{}                        = Datatype
  defnToKind   TCM.Record{}                          = Record
  defnToKind   TCM.Constructor{ TCM.conSrcCon = c }  = Constructor $ I.conInductive c
  defnToKind   TCM.Primitive{}                       = Primitive
  defnToKind   TCM.PrimitiveSort{}                   = Primitive
  defnToKind   TCM.AbstractDefn{}                    = __IMPOSSIBLE__

  con :: NameKind
  con = Constructor ASP.Inductive
type NameKindMap     = HashMap A.QName NameKind

data NameKindBuilder = NameKindBuilder
  { runBuilder :: NameKindMap -> NameKindMap
  }

mergeNameKind :: NameKind -> NameKind -> NameKind
mergeNameKind Postulate k = k
mergeNameKind _     Macro = Macro  -- If the abstract syntax says macro, it's a macro.
mergeNameKind k         _ = k



instance Semigroup (NameKindBuilder) where
  NameKindBuilder f <> NameKindBuilder g = NameKindBuilder $ f . g
instance Monoid (NameKindBuilder) where
  mempty = NameKindBuilder id
  mappend = (<>)
instance Singleton KName NameKindBuilder where
  singleton (WithKind k q) = NameKindBuilder $
    HMap.insertWith mergeNameKind q $ kindOfNameToNameKind k
instance Collection KName NameKindBuilder
