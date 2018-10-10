{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Haskus.Utils.EADT.TH
   ( eadtConstructor
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
-- > $(eadtConstructor 'ConsF "Cons")
-- >
-- > ====>
-- >
-- > pattern Cons :: ConsF a :<: xs => a -> EADT xs -> EADT xs
-- > pattern Cons a l = VF (ConsF a l)
--
eadtConstructor
   :: Name     -- ^ Actual constructor (e.g., ConsF)
   -> String   -- ^ Name of the pattern (e.g., Cons)
   -> Q [Dec]
eadtConstructor consName patStr = do
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

         -- make pattern type
         xsName <- newName "xs"
         let xs = VarT xsName
         eadtXs <- [t| EADT $(return xs) |]

         let
            -- [* -> *]
            tyToTyList = AppT ListT (AppT (AppT ArrowT StarT) StarT)
            -- remove functor var; add "xs" var
            tvs'       = init tvs ++ [KindedTV xsName tyToTyList]
            -- retreive functor var in "e"
            KindedTV e StarT = last tvs

            -- replace functor variable with EADT type
            go (VarT x :->: b)
               | x == e      = eadtXs :->: go b
            go (a :->: b)    = a :->: go b
            go _             = eadtXs
            t'               = go tys

            getConTyp (_ :->: b) = getConTyp b
            getConTyp (AppT a _) = a -- remove last AppT (functor var)
            getConTyp _          = error "Invalid constructor type"

            conTyp = getConTyp tys

         prd <-  [t| $(return conTyp) :<: $(return xs) |]

         let sig = PatSynSigD patName (ForallT tvs' [prd] t')

         return [sig,pat]

      _ -> fail $ show consName ++ "'s type doesn't have a free variable, it can't be a functor"


pattern (:->:) :: Type -> Type -> Type
pattern a :->: b = AppT (AppT ArrowT a) b
