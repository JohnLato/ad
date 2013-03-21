{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Identity
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Identity
    ( Id(..)
    , probe
    , unprobe
    , probed
    , unprobed
    ) where

import Control.Applicative
import Data.Data (Data)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid
import Data.Number.Erf
import Data.Typeable (Typeable)
import Data.Traversable (Traversable, traverse)
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Types

newtype Id a = Id { runId :: a } deriving
    (Iso a, Eq, Ord, Show, Enum, Bounded, Num, Real, Fractional, Floating, RealFrac, RealFloat, Monoid, Data, Typeable)

type instance Domain Id a = ()

probe :: a -> AD Id s a
probe a = AD (Id a)

unprobe :: AD Id s a -> a
unprobe (AD (Id a)) = a

pid :: f a -> f (Id a)
pid = iso

unpid :: f (Id a) -> f a
unpid = osi

probed :: f a -> f (AD Id s a)
probed = iso . pid

unprobed :: f (AD Id s a) -> f a
unprobed = unpid . osi

instance Functor Id where
    fmap f (Id a) = Id (f a)

instance Foldable Id where
    foldMap f (Id a) = f a

instance Traversable Id where
    traverse f (Id a) = Id <$> f a

instance Applicative Id where
    pure = Id
    Id f <*> Id a = Id (f a)

instance Monad Id where
    return = Id
    Id a >>= f = f a

instance Lifted Id where
    (==!) = (==)
    compare1 = compare
    showsPrec1 = showsPrec
    fromInteger1 = fromInteger
    (+!) = (+)
    (-!) = (-)
    (*!) = (*)
    negate1 = negate
    abs1 = abs
    signum1 = signum
    (/!) = (/)
    recip1 = recip
    fromRational1 = fromRational
    toRational1 = toRational
    pi1 = pi
    exp1 = exp
    log1 = log
    sqrt1 = sqrt
    (**!) = (**)
    logBase1 = logBase
    sin1 = sin
    cos1 = cos
    tan1 = tan
    asin1 = asin
    acos1 = acos
    atan1 = atan
    sinh1 = sinh
    cosh1 = cosh
    tanh1 = tanh
    asinh1 = asinh
    acosh1 = acosh
    atanh1 = atanh
    properFraction1 = properFraction
    truncate1 = truncate
    round1 = round
    ceiling1 = ceiling
    floor1 = floor
    floatRadix1 = floatRadix
    floatDigits1 = floatDigits
    floatRange1 = floatRange
    decodeFloat1 = decodeFloat
    encodeFloat1 = encodeFloat
    exponent1 = exponent
    significand1 = significand
    scaleFloat1 = scaleFloat
    isNaN1 = isNaN
    isInfinite1 = isInfinite
    isDenormalized1 = isDenormalized
    isNegativeZero1 = isNegativeZero
    isIEEE1 = isIEEE
    atan21 = atan2
    succ1 = succ
    pred1 = pred
    toEnum1 = toEnum
    fromEnum1 = fromEnum
    enumFrom1 = enumFrom
    enumFromThen1 = enumFromThen
    enumFromTo1 = enumFromTo
    enumFromThenTo1 = enumFromThenTo
    minBound1 = minBound
    maxBound1 = maxBound
    erf1 = erf
    erfc1 = erfc
    normcdf1 = normcdf
    inverf1 = inverf
    inverfc1 = inverfc
    invnormcdf1 = invnormcdf

instance Mode Id where
    auto = Id
    Id a ^* b = Id (a * b)
    a *^ Id b = Id (a * b)
    Id a <+> Id b = Id (a + b)
    Id a <**> Id b = Id (a ** b)

instance Primal Id where
    primal (Id a) = a

instance Erf a => Erf (Id a) where
  erf = Id . erf . runId
  erfc = Id . erfc . runId
  normcdf = Id . normcdf . runId

instance InvErf a => InvErf (Id a) where
  inverf = Id . inverf . runId
  inverfc = Id . inverfc . runId
  invnormcdf = Id . invnormcdf . runId
