{-# LANGUAGE BangPatterns, ConstraintKinds, TemplateHaskell, TypeFamilies, TypeOperators, FlexibleContexts, UndecidableInstances, DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.AD.Internal.Sparse
    ( Index(..)
    , emptyIndex
    , addToIndex
    , indices
    , Sparse(..)
    , apply
    , vars
    , d, d', ds
    , skeleton
    , spartial
    , partial
    , vgrad
    , vgrad'
    , vgrads
    , Grad(..)
    , Grads(..)
    ) where

import Prelude hiding (lookup)
import Control.Applicative hiding ((<**>))
import Numeric.AD.Internal.Classes
import Control.Comonad.Cofree
import Numeric.AD.Internal.Types
import Data.Data
import Data.Typeable ()
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap, mapWithKey, unionWith, findWithDefault, toAscList, singleton, insertWith, lookup)
import Data.Traversable
import Language.Haskell.TH

newtype Index = Index (IntMap Int)

emptyIndex :: Index
emptyIndex = Index IntMap.empty
{-# INLINE emptyIndex #-}

addToIndex :: Int -> Index -> Index
addToIndex k (Index m) = Index (insertWith (+) k 1 m)
{-# INLINE addToIndex #-}

indices :: Index -> [Int]
indices (Index as) = uncurry (flip replicate) `concatMap` toAscList as
{-# INLINE indices #-}

-- | We only store partials in sorted order, so the map contained in a partial
-- will only contain partials with equal or greater keys to that of the map in
-- which it was found. This should be key for efficiently computing sparse hessians.
-- there are only (n + k - 1) choose k distinct nth partial derivatives of a
-- function with k inputs.
data Sparse a
  = Sparse !a (IntMap (Sparse a))
  | Zero
  deriving (Show, Data, Typeable)

type instance Domain Sparse a = ()

-- | drop keys below a given value
dropMap :: Int -> IntMap a -> IntMap a
dropMap n = snd . IntMap.split (n - 1)
{-# INLINE dropMap #-}

times :: Num a => Sparse a -> Int -> Sparse a -> Sparse a
times Zero _ _ = Zero
times _ _ Zero = Zero
times (Sparse a as) n (Sparse b bs) = Sparse (a * b) $
    unionWith (<+>)
        (fmap (^* b) (dropMap n as))
        (fmap (a *^) (dropMap n bs))
{-# INLINE times #-}

vars :: (Traversable f, Num a) => f a -> f (AD Sparse s a)
vars = snd . mapAccumL var 0
    where
        var !n a = (n + 1, AD $ Sparse a $ singleton n $ auto 1)
{-# INLINE vars #-}

apply :: (Traversable f, Num a) => (f (AD Sparse s a) -> b) -> f a -> b
apply f = f . vars
{-# INLINE apply #-}

skeleton :: Traversable f => f a -> f Int
skeleton = snd . mapAccumL (\ !n _ -> (n + 1, n)) 0
{-# INLINE skeleton #-}

d :: (Traversable f, Num a) => f b -> AD Sparse s a -> f a
d fs (AD Zero) = 0 <$ fs
d fs (AD (Sparse _ da)) = snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs
{-# INLINE d #-}

d' :: (Traversable f, Num a) => f a -> AD Sparse s a -> (a, f a)
d' fs (AD Zero) = (0, 0 <$ fs)
d' fs (AD (Sparse a da)) = (a, snd $ mapAccumL (\ !n _ -> (n + 1, maybe 0 primal $ lookup n da)) 0 fs)
{-# INLINE d' #-}

ds :: (Traversable f, Num a) => f b -> AD Sparse s a -> Cofree f a
ds fs (AD Zero) = r where r = 0 :< (r <$ fs)
ds fs (AD as@(Sparse a _)) = a :< (go emptyIndex <$> fns)
    where
        fns = skeleton fs
        -- go :: Index -> Int -> Cofree f a
        go ix i = partial (indices ix') as :< (go ix' <$> fns)
            where ix' = addToIndex i ix
{-# INLINE ds #-}

{-
vvars :: Num a => Vector a -> Vector (AD Sparse a)
vvars = Vector.imap (\n a -> AD $ Sparse a $ singleton n $ auto 1)
{-# INLINE vvars #-}

vapply :: Num a => (Vector (AD Sparse a) -> b) -> Vector a -> b
vapply f = f . vvars
{-# INLINE vapply #-}


vd :: Num a => Int -> AD Sparse a -> Vector a
vd n (AD (Sparse _ da)) = Vector.generate n $ \i -> maybe 0 primal $ lookup i da
{-# INLINE vd #-}

vd' :: Num a => Int -> AD Sparse a -> (a, Vector a)
vd' n (AD (Sparse a da)) = (a , Vector.generate n $ \i -> maybe 0 primal $ lookup i da)
{-# INLINE vd' #-}

vds :: Num a => Int -> AD Sparse a -> Cofree Vector a
vds n (AD as@(Sparse a _)) = a :< Vector.generate n (go emptyIndex)
    where
        go ix i = partial (indices ix') as :< Vector.generate n (go ix')
            where ix' = addToIndex i ix
{-# INLINE vds #-}
-}

partial :: Num a => [Int] -> Sparse a -> a
partial []     (Sparse a _)  = a
partial (n:ns) (Sparse _ da) = partial ns $ findWithDefault (auto 0) n da
partial _      Zero          = 0
{-# INLINE partial #-}

spartial :: Num a => [Int] -> Sparse a -> Maybe a
spartial [] (Sparse a _) = Just a
spartial (n:ns) (Sparse _ da) = do
    a' <- lookup n da
    spartial ns a'
spartial _  Zero         = Nothing
{-# INLINE spartial #-}

instance Primal Sparse where
    primal (Sparse a _) = a
    primal Zero = 0

instance Lifted Sparse => Mode Sparse where
    auto a = Sparse a IntMap.empty
    zero = Zero
    Zero <**> y    = auto (0 ** primal y)
    _    <**> Zero = auto 1
    x    <**> y@(Sparse b bs)
      | IntMap.null bs = lift1 (**b) (\z -> b *^ z <**> Sparse (b-1) IntMap.empty) x
      | otherwise      = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y
    Zero <+> a = a
    a <+> Zero = a
    Sparse a as <+> Sparse b bs = Sparse (a + b) $ unionWith (<+>) as bs
    Zero        ^* _ = Zero
    Sparse a as ^* b = Sparse (a * b) $ fmap (^* b) as
    _ *^ Zero        = Zero
    a *^ Sparse b bs = Sparse (a * b) $ fmap (a *^) bs
    Zero        ^/ _ = Zero
    Sparse a as ^/ b = Sparse (a / b) $ fmap (^/ b) as

instance Lifted Sparse => Jacobian Sparse where
    type D Sparse = Sparse
    unary f _ Zero = auto (f 0)
    unary f dadb (Sparse pb bs) = Sparse (f pb) $ mapWithKey (times dadb) bs

    lift1 f _ Zero = auto (f 0)
    lift1 f df b@(Sparse pb bs) = Sparse (f pb) $ mapWithKey (times (df b)) bs

    lift1_ f _  Zero = auto (f 0)
    lift1_ f df b@(Sparse pb bs) = a where
        a = Sparse (f pb) $ mapWithKey (times (df a b)) bs

    binary f _    _    Zero           Zero           = auto (f 0 0)
    binary f _    dadc Zero           (Sparse pc dc) = Sparse (f 0  pc) $ mapWithKey (times dadc) dc
    binary f dadb _    (Sparse pb db) Zero           = Sparse (f pb 0 ) $ mapWithKey (times dadb) db
    binary f dadb dadc (Sparse pb db) (Sparse pc dc) = Sparse (f pb pc) $
        unionWith (<+>)
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

    lift2 f _  Zero             Zero = auto (f 0 0)
    lift2 f df Zero c@(Sparse pc dc) = Sparse (f 0 pc) $ mapWithKey (times dadc) dc where dadc = snd (df zero c)
    lift2 f df b@(Sparse pb db) Zero = Sparse (f pb 0) $ mapWithKey (times dadb) db where dadb = fst (df b zero)
    lift2 f df b@(Sparse pb db) c@(Sparse pc dc) = Sparse (f pb pc) da where
        (dadb, dadc) = df b c
        da = unionWith (<+>)
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

    lift2_ f _  Zero             Zero = auto (f 0 0)
    lift2_ f df b@(Sparse pb db) Zero = a where a = Sparse (f pb 0) (mapWithKey (times (fst (df a b zero))) db)
    lift2_ f df Zero c@(Sparse pc dc) = a where a = Sparse (f 0 pc) (mapWithKey (times (snd (df a zero c))) dc)
    lift2_ f df b@(Sparse pb db) c@(Sparse pc dc) = a where
        (dadb, dadc) = df a b c
        a = Sparse (f pb pc) da
        da = unionWith (<+>)
            (mapWithKey (times dadb) db)
            (mapWithKey (times dadc) dc)

deriveLifted id $ conT ''Sparse


class Num a => Grad i o o' a | i -> a o o', o -> a i o', o' -> a i o where
    pack :: i -> [AD Sparse () a] -> AD Sparse () a
    unpack :: ([a] -> [a]) -> o
    unpack' :: ([a] -> (a, [a])) -> o'

instance Num a => Grad (AD Sparse () a) [a] (a, [a]) a where
    pack i _ = i
    unpack f = f []
    unpack' f = f []

instance Grad i o o' a => Grad (AD Sparse () a -> i) (a -> o) (a -> o') a where
    pack f (a:as) = pack (f a) as
    pack _ [] = error "Grad.pack: logic error"
    unpack f a = unpack (f . (a:))
    unpack' f a = unpack' (f . (a:))

vgrad :: Grad i o o' a => i -> o
vgrad i = unpack (unsafeGrad (pack i))
    where
        unsafeGrad f as = d as $ apply f as
{-# INLINE vgrad #-}

vgrad' :: Grad i o o' a => i -> o'
vgrad' i = unpack' (unsafeGrad' (pack i))
    where
        unsafeGrad' f as = d' as $ apply f as
{-# INLINE vgrad' #-}

class Num a => Grads i o a | i -> a o, o -> a i where
    packs :: i -> [AD Sparse () a] -> AD Sparse () a
    unpacks :: ([a] -> Cofree [] a) -> o

instance Num a => Grads (AD Sparse () a) (Cofree [] a) a where
    packs i _ = i
    unpacks f = f []

instance Grads i o a => Grads (AD Sparse () a -> i) (a -> o) a where
    packs f (a:as) = packs (f a) as
    packs _ [] = error "Grad.pack: logic error"
    unpacks f a = unpacks (f . (a:))

vgrads :: Grads i o a => i -> o
vgrads i = unpacks (unsafeGrads (packs i))
    where
        unsafeGrads f as = ds as $ apply f as
{-# INLINE vgrads #-}

