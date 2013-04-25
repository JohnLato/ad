{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures, Rank2Types, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, PatternGuards, CPP #-}
{-# LANGUAGE FlexibleContexts, FunctionalDependencies, UndecidableInstances, GeneralizedNewtypeDeriving, TemplateHaskell #-}
-- {-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Classes
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Classes
    (
    -- * AD modes
      Mode(..)
    , one
    -- * Automatically Deriving AD
    , Jacobian(..)
    , Primal(..)
    , deriveNumeric
    , Iso(..)
    , Scalar
    ) where

import Control.Applicative ((<$>), pure)
import Control.Monad
import Data.Number.Erf
import Data.Proxy
import Language.Haskell.TH.Syntax

type family Scalar t

class Iso a b where
    iso :: f a -> f b
    osi :: f b -> f a

instance Iso a a where
    iso = id
    osi = id

infixr 6 <+>
infixr 7 *^
infixl 7 ^*
infixr 7 ^/
infixr 8 <**>

class (Num (Scalar t), Num t) => Mode t where
    -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
    isKnownConstant :: t -> Bool
    isKnownConstant _ = False

    -- | allowed to return False for zero, but we give more NaN's than strictly necessary then
    isKnownZero :: t -> Bool
    isKnownZero _ = False

    -- | Embed a constant
    auto  :: Scalar t -> t

    -- | Vector sum
    (<+>) :: t -> t -> t

    -- | Scalar-vector multiplication
    (*^) :: Scalar t -> t -> t

    -- | Vector-scalar multiplication
    (^*) :: t -> Scalar t -> t

    -- | Scalar division
    (^/) :: (Num t, Fractional (Scalar t)) => t -> Scalar t -> t

    -- | Exponentiation, this should be overloaded if you can figure out anything about what is constant!
    (<**>) :: (Floating (Scalar t)) => t -> t -> t

    -- default (<**>) :: (Jacobian t, Floating (D t), Floating (Scalar t)) => t -> t -> t
    -- x <**> y = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

    -- | > 'zero' = 'lift' 0
    zero :: t

#ifndef HLINT
    default (*^) :: Num t => Scalar t -> t -> t
    a *^ b = auto a * b
    default (^*) :: Num t => t -> Scalar t -> t
    a ^* b = a * auto b
#endif

    a ^/ b = a ^* recip b

    zero = auto 0

one :: Mode t => t
one = auto 1
{-# INLINE one #-}

negOne :: Mode t => t
negOne = auto (-1)
{-# INLINE negOne #-}

-- | 'Primal' is used by 'deriveMode' but is not exposed
-- via the 'Mode' class to prevent its abuse by end users
-- via the AD data type.
--
-- It provides direct access to the result, stripped of its derivative information,
-- but this is unsafe in general as (auto . primal) would discard derivative
-- information. The end user is protected from accidentally using this function
-- by the universal quantification on the various combinators we expose.

class Primal t where
    primal :: t -> Scalar t

-- | 'Jacobian' is used by 'deriveMode' but is not exposed
-- via 'Mode' to prevent its abuse by end users
-- via the 'AD' data type.
class (Mode t, Mode (D t), Num (D t)) => Jacobian t where
    type D t :: *

    unary  :: (Scalar t -> Scalar t) -> D t -> t -> t
    lift1  :: (Scalar t -> Scalar t) -> (D t -> D t) -> t -> t
    lift1_ :: (Scalar t -> Scalar t) -> (D t -> D t -> D t) -> t -> t

    binary :: (Scalar t -> Scalar t -> Scalar t) -> D t -> D t -> t -> t -> t
    lift2  :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> (D t, D t)) -> t -> t -> t
    lift2_ :: (Scalar t -> Scalar t -> Scalar t) -> (D t -> D t -> D t -> (D t, D t)) -> t -> t -> t

withPrimal :: (Jacobian t, Scalar t ~ Scalar (D t)) => t -> Scalar t -> t
withPrimal t a = unary (const a) one t
{-# INLINE withPrimal #-}

fromBy :: (Jacobian t, Scalar t ~ Scalar (D t)) => t -> t -> Int -> Scalar t -> t
fromBy a delta n x = binary (\_ _ -> x) one (fromIntegral n) a delta

discrete1 :: Primal t => (Scalar t -> c) -> t -> c
discrete1 f x = f (primal x)
{-# INLINE discrete1 #-}

discrete2 :: Primal t => (Scalar t -> Scalar t -> c) -> t -> t -> c
discrete2 f x y = f (primal x) (primal y)
{-# INLINE discrete2 #-}

discrete3 :: Primal t => (Scalar t -> Scalar t -> Scalar t -> d) -> t -> t -> t -> d
discrete3 f x y z = f (primal x) (primal y) (primal z)
{-# INLINE discrete3 #-}

-- | @'deriveNumeric g s@ provides the following instances:
--
-- > instance ('Num' a, 'Enum' a) => 'Enum' ($g a s)
-- > instance ('Num' a, 'Eq' a) => 'Eq' ($g a s)
-- > instance ('Num' a, 'Ord' a) => 'Ord' ($g a s)
-- > instance ('Num' a, 'Bounded' a) => 'Bounded' ($g a s)
--
-- > instance ('Num' a) => 'Num' ($g a s)
-- > instance ('Fractional' a) => 'Fractional' ($g a s)
-- > instance ('Floating' a) => 'Floating' ($g a s)
-- > instance ('RealFloat' a) => 'RealFloat' ($g a s)
-- > instance ('RealFrac' a) => 'RealFrac' ($g a s)
-- > instance ('Real' a) => 'Real' ($g a s)
deriveNumeric :: ([Pred] -> [Pred]) -> Type -> Type -> Q [Dec]
deriveNumeric f tCon s' = map fudgeCxt <$> lifted
    where
      t = pure tCon
      -- s = pure s'
      fudgeCxt (InstanceD cxt typ dec) = InstanceD (f cxt) typ dec
      fudgeCxt _ = error "Numeric.AD.Internal.Classes.deriveNumeric_fudgeCxt: Not InstanceD"
      lifted = [d|
       instance Num a => Num ($t a) where
        fromInteger 0  = zero
        fromInteger n = auto (fromInteger n)
        (+)          = (<+>) -- binary (+) one one
        (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
        (*)          = lift2 (*) (\x y -> (y, x))
        negate       = lift1 negate (const (auto (-1)))
        abs          = lift1 abs signum
        signum a     = lift1 signum (const zero) a
       instance Fractional a => Fractional ($t a) where
        fromRational 0 = zero
        fromRational r = auto (fromRational r)
        x / y        = x * recip y
        recip        = lift1_ recip (const . negate . join (*))
       instance Floating a => Floating ($t a) where
        pi       = auto pi
        exp      = lift1_ exp const
        log      = lift1 log recip
        logBase x y = log y / log x
        sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
        (**)     = (<**>)
        --x ** y
        --   | isKnownZero y     = 1
        --   | isKnownConstant y, y' <- primal y = lift1 (** y') ((y'*) . (**(y'-1))) x
        --   | otherwise         = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log1 xi)) x y
        sin      = lift1 sin cos
        cos      = lift1 cos $ negate . sin
        tan      = lift1 tan $ recip . join (*) . cos
        asin     = lift1 asin $ \x -> recip (sqrt (auto 1 - join (*) x))
        acos     = lift1 acos $ \x -> negate (recip (sqrt (one - join (*) x)))
        atan     = lift1 atan $ \x -> recip (one + join (*) x)
        sinh     = lift1 sinh cosh
        cosh     = lift1 cosh sinh
        tanh     = lift1 tanh $ recip . join (*) . cosh
        asinh    = lift1 asinh $ \x -> recip (sqrt (one + join (*) x))
        acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - one))
        atanh    = lift1 atanh $ \x -> recip (one - join (*) x)
       |]
