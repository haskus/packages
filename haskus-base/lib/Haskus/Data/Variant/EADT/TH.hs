{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

-- | Template-Haskell helpers for EADTs
module Haskus.Data.Variant.EADT.TH
   ( eadtPattern
   , eadtInfixPattern
   , eadtPatternT
   , eadtInfixPatternT
   )
where

import Language.Haskell.TH
import Control.Monad
import Haskus.Data.Variant.EADT

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
eadtPattern consName patStr = eadtPattern' consName patStr Nothing False

-- | Create an infix pattern synonym for an EADT constructor
--
-- E.g.
--
-- > data ConsF a e = ConsF a e deriving (Functor)
-- > $(eadtInfixPattern 'ConsF ":->")
-- >
-- > ====>
-- >
-- > pattern (:->) :: ConsF a :<: xs => a -> EADT xs -> EADT xs
-- > pattern a :-> l = VF (ConsF a l)
--
eadtInfixPattern
   :: Name       -- ^ Actual constructor (e.g., ConsF)
   -> String     -- ^ Name of the pattern (e.g., Cons)
   -> Q [Dec]
eadtInfixPattern consName patStr = eadtPattern' consName patStr Nothing True

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
   eadtPattern' consName patStr (Just qtype) False

-- | Like `eadtPatternT` but generating an infix pattern synonym
eadtInfixPatternT
   :: Name       -- ^ Actual constructor (e.g., ConsF)
   -> String     -- ^ Name of the pattern (e.g., Cons)
   -> Q Type     -- ^ Type of the EADT (e.g., [t|forall a. List a|])
   -> Q [Dec]
eadtInfixPatternT consName patStr qtype =
   eadtPattern' consName patStr (Just qtype) True

-- | Create a pattern synonym for an EADT constructor
eadtPattern'
   :: Name       -- ^ Actual constructor (e.g., ConsF)
   -> String     -- ^ Name of the pattern (e.g., Cons)
   -> Maybe (Q Type) -- ^ EADT type
   -> Bool       -- ^ Declare infix pattern
   -> Q [Dec]
eadtPattern' consName patStr mEadtTy isInfix = do
   let patName = mkName patStr

   typ <- reify consName >>= \case
            DataConI _ t _ -> return t
            _              -> fail $ show consName ++ " isn't a data constructor"

   case typ of
      ForallT tvs _ tys -> do
         -- make pattern
         let getConArity = \case
               AppT (AppT ArrowT _a) b              -> 1 + getConArity b
#if MIN_VERSION_base(4,15,0)
               AppT (AppT (AppT MulArrowT _m) _a) b -> 1 + getConArity b
#endif
               _                                    -> 0

             conArity = getConArity tys
         conArgs <- replicateM conArity (newName "c")

         let vf     = mkName "Haskus.Data.Variant.EADT.VF"

         args <- if not isInfix
            then return (PrefixPatSyn conArgs)
            else case conArgs of
                  [x,y] -> return (InfixPatSyn x y)
                  xs    -> fail $ "Infix pattern should have exactly two parameters (found " ++ show (length xs) ++ ")"

         let pat    = PatSynD patName args ImplBidir
#if MIN_VERSION_base(4,16,0)
                         -- handle new field for type-applications in patterns
                         (ConP vf [] [ConP consName [] (fmap VarP conArgs)])
#else
                         (ConP vf [ConP consName (fmap VarP conArgs)])
#endif

         let
            -- retrieve constructor type without the functor var
            -- e.g. ConsF a for ConsF a e
            getConTyp (AppT (AppT ArrowT _a) b)              = getConTyp b
#if MIN_VERSION_base(4,15,0)
            getConTyp (AppT (AppT (AppT MulArrowT _m) _a) b) = getConTyp b
#endif
            getConTyp (AppT a _) = a -- remove last AppT (functor var)
            getConTyp _          = error "Invalid constructor type"

            conTyp = getConTyp tys

            -- [* -> *]
            tyToTyList = AppT ListT (AppT (AppT ArrowT StarT) StarT)

            -- retrieve functor var in "e"
#if MIN_VERSION_base(4,16,0)
            e = case last tvs of
              KindedTV nm _ _ -> nm
              PlainTV nm _    -> nm
#elif MIN_VERSION_base(4,15,0)
            KindedTV e _ StarT = last tvs
#else
            KindedTV e StarT = last tvs
#endif


         -- make pattern type
         (newTvs,eadtTy,ctx) <- do
            xsName <- newName "xs"
            let
               xs = VarT xsName
#if MIN_VERSION_base(4,15,0)
               xsTy = KindedTV xsName SpecifiedSpec tyToTyList
#else
               xsTy = KindedTV xsName tyToTyList
#endif
            eadtXs <- [t| EADT $(return xs) |]

            prd <-  [t| $(return conTyp) :<: $(return xs) |]
            prd2 <-  [t| $(return (VarT e)) ~ $(return eadtXs) |]
            case mEadtTy of
               Nothing -> return ([xsTy],eadtXs,[prd,prd2])
               Just ty -> do
                  ty' <- ty
                  let (tvs',ty'',ctx') = case ty' of
                        -- put freevars of the user specified type with the
                        -- other ones
                        ForallT tvs'' ctx'' t -> (tvs'',t,ctx'')
                        _                     -> ([],ty',[])
                  prd3 <- [t| $(return ty'') ~ $(return eadtXs) |]
                  return (xsTy:tvs',ty'',prd:prd2:prd3:ctx')

         let
            -- remove functor var; add new type var
            tvs'       = tvs ++ newTvs

            -- replace functor variable with EADT type
            go (AppT (AppT ArrowT a) b)
               | VarT v <- a
               , v == e      = AppT (AppT ArrowT eadtTy) (go b)
               | otherwise   = AppT (AppT ArrowT a)      (go b)
#if MIN_VERSION_base(4,15,0)
            go (AppT (AppT (AppT MulArrowT _m) a) b)
               | VarT v <- a
               -- Linear types don't support pattern synonyms (GHC#18806)
               -- Use normal arrows instead.
               , v == e      = AppT (AppT ArrowT eadtTy) (go b)
               | otherwise   = AppT (AppT ArrowT a)      (go b)
#endif
            go _             = eadtTy
            t'               = go tys


         let sig = PatSynSigD patName (ForallT tvs' ctx t')

         return [sig,pat]

      _ -> fail $ show consName ++ "'s type doesn't have a free variable, it can't be a functor"
