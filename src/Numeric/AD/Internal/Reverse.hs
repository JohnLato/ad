{-# LANGUAGE ConstraintKinds, CPP, Rank2Types, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, TemplateHaskell, GADTs, TypeFamilies, DeriveDataTypeable, FlexibleContexts #-}
-- {-# OPTIONS_HADDOCK hide, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Reverse
-- Copyright   :  (c) Edward Kmett 2012
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Reverse-Mode Automatic Differentiation using a single Wengert list (or \"tape\").
--
-- This version uses @Data.Reflection@ to find and update the tape.
--
-- This is asymptotically faster than using @Reverse@, which
-- is forced to reify and topologically sort the graph, but it requires
-- a fairly expensive rendezvous during construction when updated using
-- multiple threads.
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Reverse
    ( Reverse(..)
    , Tape(..)
    , Head(..)
    , Cells(..)
    , reifyTape
    , partials
    , partialArrayOf
    , partialMapOf
    , derivativeOf
    , derivativeOf'
    ) where

import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.Array.Unsafe as Unsafe
import Data.IORef
import Data.IntMap (IntMap, fromDistinctAscList)
import Data.Proxy
import Data.Reflection
import Data.Typeable
import Language.Haskell.TH hiding (reify)
import Numeric.AD.Internal.Types
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity
import Numeric.AD.Internal.Var
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- evil untyped tape
#ifndef HLINT
data Cells where
  Nil    :: Cells
  Unary  :: {-# UNPACK #-} !Int -> a -> Cells -> Cells
  Binary :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> a -> a -> Cells -> Cells
#endif

dropCells :: Int -> Cells -> Cells
dropCells 0 xs = xs
dropCells _ Nil = Nil
dropCells n (Unary _ _ xs)      = (dropCells $! n - 1) xs
dropCells n (Binary _ _ _ _ xs) = (dropCells $! n - 1) xs

data Head = Head {-# UNPACK #-} !Int Cells

newtype Tape = Tape { getTape :: IORef Head }

un :: Int -> a -> Head -> (Head, Int)
un i di (Head r t) = h `seq` r' `seq` (h, r') where
  r' = r + 1
  h = Head r' (Unary i di t)
{-# INLINE un #-}

bin :: Int -> Int -> a -> a -> Head -> (Head, Int)
bin i j di dj (Head r t) = h `seq` r' `seq` (h, r') where
  r' = r + 1
  h = Head r' (Binary i j di dj t)
{-# INLINE bin #-}

modifyTape :: Reifies s Tape => p s -> (Head -> (Head, r)) -> IO r
modifyTape p = atomicModifyIORef (getTape (reflect p))
{-# INLINE modifyTape #-}

-- | This is used to create a new entry on the chain given a unary function, its derivative with respect to its input,
-- the variable ID of its input, and the value of its input. Used by 'unary' and 'binary' internally.
unarily :: forall s a. Reifies s Tape => (a -> a) -> a -> Int -> a -> Reverse s a
unarily f di i b = Reverse (unsafePerformIO (modifyTape (Proxy :: Proxy s) (un i di))) $! f b
{-# INLINE unarily #-}

-- | This is used to create a new entry on the chain given a binary function, its derivatives with respect to its inputs,
-- their variable IDs and values. Used by 'binary' internally.
binarily :: forall s a. Reifies s Tape => (a -> a -> a) -> a -> a -> Int -> a -> Int -> a -> Reverse s a
binarily f di dj i b j c = Reverse (unsafePerformIO (modifyTape (Proxy :: Proxy s) (bin i j di dj))) $! f b c
{-# INLINE binarily #-}

#ifndef HLINT
data Reverse s a where
  Zero :: Reverse s a
  Lift :: a -> Reverse s a
  Reverse :: {-# UNPACK #-} !Int -> a -> Reverse s a
  deriving (Show, Typeable)
#endif

type instance Domain (Reverse s) a = ()

instance (Reifies s Tape, Lifted (Reverse s)) => Mode (Reverse s) where
  isKnownZero Zero = True
  isKnownZero _    = False

  isKnownConstant Reverse{} = False
  isKnownConstant _ = True

  auto = Lift
  zero = Zero
  (<+>)  = binary (+) one one
  a *^ b = lift1 (a *) (\_ -> auto a) b
  a ^* b = lift1 (* b) (\_ -> auto b) a
  a ^/ b = lift1 (/ b) (\_ -> auto (recip b)) a

  Zero <**> y      = auto (0 ** primal y)
  _    <**> Zero   = auto 1
  x    <**> Lift y = lift1 (**y) (\z -> y *^ z ** Id (y - 1)) x
  x    <**> y      = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y

instance Primal (Reverse s) where
    primal Zero = 0
    primal (Lift a) = a
    primal (Reverse _ a) = a

instance (Reifies s Tape, Lifted (Reverse s)) => Jacobian (Reverse s) where
    type D (Reverse s) = Id

    unary f _         (Zero)   = Lift (f 0)
    unary f _         (Lift a) = Lift (f a)
    unary f (Id dadi) (Reverse i b) = unarily f dadi i b

    lift1 f df b = unary f (df (Id pb)) b
        where pb = primal b

    lift1_ f df b = unary (const a) (df (Id a) (Id pb)) b
        where pb = primal b
              a = f pb

    binary f _         _         Zero     Zero     = Lift (f 0 0)
    binary f _         _         Zero     (Lift c) = Lift (f 0 c)
    binary f _         _         (Lift b) Zero     = Lift (f b 0)
    binary f _         _         (Lift b) (Lift c) = Lift (f b c)

    binary f _         (Id dadc) Zero        (Reverse i c) = unarily (f 0) dadc i c
    binary f _         (Id dadc) (Lift b)    (Reverse i c) = unarily (f b) dadc i c
    binary f (Id dadb) _         (Reverse i b) Zero        = unarily (`f` 0) dadb i b
    binary f (Id dadb) _         (Reverse i b) (Lift c)    = unarily (`f` c) dadb i b
    binary f (Id dadb) (Id dadc) (Reverse i b) (Reverse j c) = binarily f dadb dadc i b j c

    lift2 f df b c = binary f dadb dadc b c
        where (dadb, dadc) = df (Id (primal b)) (Id (primal c))

    lift2_ f df b c = binary (\_ _ -> a) dadb dadc b c
        where
            pb = primal b
            pc = primal c
            a = f pb pc
            (dadb, dadc) = df (Id a) (Id pb) (Id pc)

let s = varT (mkName "s") in
  deriveLifted (classP ''Reifies [s, conT ''Tape] :) (conT ''Reverse `appT` s)

-- | Helper that extracts the derivative of a chain when the chain was constructed with one variable.
derivativeOf :: (Reifies s Tape, Num a) => Proxy s -> AD (Reverse s) r a -> a
derivativeOf _ = sum . partials
{-# INLINE derivativeOf #-}

-- | Helper that extracts both the primal and derivative of a chain when the chain was constructed with one variable.
derivativeOf' :: (Reifies s Tape, Num a) => Proxy s -> AD (Reverse s) r a -> (a, a)
derivativeOf' p r = (primal r, derivativeOf p r)
{-# INLINE derivativeOf' #-}

-- | Used internally to push sensitivities down the chain.
backPropagate :: Num a => Int -> Cells -> STArray s Int a -> ST s Int
backPropagate k Nil _ = return k
backPropagate k (Unary i g xs) ss = do
  da <- readArray ss k
  db <- readArray ss i
  writeArray ss i $! db + unsafeCoerce g*da
  (backPropagate $! k - 1) xs ss
backPropagate k (Binary i j g h xs) ss = do
  da <- readArray ss k
  db <- readArray ss i
  writeArray ss i $! db + unsafeCoerce g*da
  dc <- readArray ss j
  writeArray ss j $! dc + unsafeCoerce h*da
  (backPropagate $! k - 1) xs ss

-- | Extract the partials from the current chain for a given AD variable.
{-# SPECIALIZE partials :: Reifies s Tape => AD (Reverse s) r Double -> [Double] #-}
partials :: forall r s a. (Reifies s Tape, Num a) => AD (Reverse s) r a -> [a]
partials (AD Zero)        = []
partials (AD (Lift _))    = []
partials (AD (Reverse k _)) = map (sensitivities !) [0..vs] where
   Head n t = unsafePerformIO $ readIORef (getTape (reflect (Proxy :: Proxy s)))
   tk = dropCells (n - k) t
   (vs,sensitivities) = runST $ do
     ss <- newArray (0, k) 0
     writeArray ss k 1
     v <- backPropagate k tk ss
     as <- Unsafe.unsafeFreeze ss
     return (v, as)

-- | Return an 'Array' of 'partials' given bounds for the variable IDs.
partialArrayOf :: (Reifies s Tape, Num a) => Proxy s -> (Int, Int) -> AD (Reverse s) r a -> Array Int a
partialArrayOf _ vbounds = accumArray (+) 0 vbounds . zip [0..] . partials
{-# INLINE partialArrayOf #-}

-- | Return an 'IntMap' of sparse partials
partialMapOf :: (Reifies s Tape, Num a) => Proxy s -> AD (Reverse s) r a -> IntMap a
partialMapOf _ = fromDistinctAscList . zip [0..] . partials
{-# INLINE partialMapOf #-}

-- | Construct a tape that starts with @n@ variables.
reifyTape :: Int -> (forall s. Reifies s Tape => Proxy s -> r) -> r
reifyTape vs k = unsafePerformIO $ do
  h <- newIORef (Head vs Nil)
  return (reify (Tape h) k)
{-# NOINLINE reifyTape #-}

instance Var (Reverse s) where
    var a v = Reverse v a
    varId (Reverse v _) = v
    varId _ = error "varId: not a Var"
