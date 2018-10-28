{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Template-Haskell helpers for EADTs
module Haskus.Utils.EADT.TH
   ( eadtPattern
   , eadtPatternT
   )
where

import Language.Haskell.TH
import Control.Monad
import Haskus.Utils.EADT

-- | Create a pattern synonym for an EADT constructor
--
-- E.g.
--
-- > data ConsF a e = ConsF a e deriving (Functor)
-- > $(eadtPattern 'ConsF "Cons")
-- >
-- > ====>
-- >
-- > pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
-- > pattern Cons a l = VF (ConsF a l)
--
eadtPattern
   :: Name       -- ^ Actual constructor (e.g., ConsF)
   -> String     -- ^ Name of the pattern (e.g., Cons)
   -> Q [Dec]
eadtPattern consName patStr = eadtPattern' consName patStr Nothing

-- | Create a pattern synonym for an EADT constructor that is part of a
-- specified EADT.
--
-- This can be useful to help the type inference because instead of using a
-- generic "EADT xs" type, the pattern uses the provided type.
--
-- E.g.
--
-- > data ConsF a e = ConsF a e deriving (Functor)
-- > data NilF    e = NilF      deriving (Functor)
-- >
-- > type List a = EADT '[ConsF a, NilF]
-- >
-- > $(eadtPatternT 'ConsF "ConsList" [t|forall a. List a|])
-- >
-- > ====>
-- >
-- > pattern ConsList ::
-- >  ( List a ~ EADT xs
-- >  , ConsF a :<: xs
-- >  ) => a -> List a -> List a
-- > pattern ConsList a l = VF (ConsF a l)
--
-- Note that you have to quantify free variables explicitly with 'forall'
--
eadtPatternT
   :: Name       -- ^ Actual constructor (e.g., ConsF)
   -> String     -- ^ Name of the pattern (e.g., Cons)
   -> Q Type     -- ^ Type of the EADT (e.g., [t|forall a. List a|])
   -> Q [Dec]
eadtPatternT consName patStr qtype =
   eadtPattern' consName patStr (Just qtype)


-- | Create a pattern synonym for an EADT constructor
eadtPattern'
   :: Name       -- ^ Actual constructor (e.g., ConsF)
   -> String     -- ^ Name of the pattern (e.g., Cons)
   -> Maybe (Q Type) -- ^ EADT type
   -> Q [Dec]
eadtPattern' consName patStr mEadtTy= do
   let patName = mkName patStr

   typ <- reify consName >>= \case
            DataConI _ t _ -> return t
            _              -> fail $ show consName ++ " isn't a data constructor"

   case typ of
      ForallT tvs _ tys -> do
         -- make pattern
         let getConArity = \case
               _ :->: b -> 1 + getConArity b
               _        -> 0

             conArity = getConArity tys
         conArgs <- replicateM conArity (newName "c")

         let vf     = mkName "Haskus.Utils.EADT.VF"

         let pat    = PatSynD patName (PrefixPatSyn conArgs) ImplBidir
                         (ConP vf [ConP consName (fmap VarP conArgs)])

         let
            -- retrieve constructor type without the functor var
            -- e.g. ConsF a for ConsF a e
            getConTyp (_ :->: b) = getConTyp b
            getConTyp (AppT a _) = a -- remove last AppT (functor var)
            getConTyp _          = error "Invalid constructor type"

            conTyp = getConTyp tys

            -- [* -> *]
            tyToTyList = AppT ListT (AppT (AppT ArrowT StarT) StarT)

         -- make pattern type
         (newTvs,eadtTy,ctx) <- do
            xsName <- newName "xs"
            let
               xs = VarT xsName
               xsTy = KindedTV xsName tyToTyList
            eadtXs <- [t| EADT $(return xs) |]

            prd <-  [t| $(return conTyp) :<: $(return xs) |]
            case mEadtTy of
               Nothing -> return ([xsTy],eadtXs,[prd])
               Just ty -> do
                  ty' <- ty
                  let (tvs',ty'',ctx') = case ty' of
                        ForallT tvs'' ctx'' t -> (tvs'',t,ctx'')
                        _                     -> ([],ty',[])
                  prd2 <- [t| $(return ty'') ~ EADT $(return xs) |]
                  return (xsTy:tvs',ty'',prd:prd2:ctx')

         let
            -- remove functor var; add new type var
            tvs'       = init tvs ++ newTvs
            -- retreive functor var in "e"
            KindedTV e StarT = last tvs

            -- replace functor variable with EADT type
            go (VarT x :->: b)
               | x == e      = eadtTy :->: go b
            go (a :->: b)    = a :->: go b
            go _             = eadtTy
            t'               = go tys


         let sig = PatSynSigD patName (ForallT tvs' ctx t')

         return [sig,pat]

      _ -> fail $ show consName ++ "'s type doesn't have a free variable, it can't be a functor"


pattern (:->:) :: Type -> Type -> Type
pattern a :->: b = AppT (AppT ArrowT a) b
