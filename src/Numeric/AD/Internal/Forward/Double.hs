{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, UndecidableInstances #-}

module Numeric.AD.Internal.Forward.Double
    ( ForwardDouble(..)
    , tangent
    , bundle
    , unbundle
    , apply
    , bind
    , bind'
    , bindWith
    , bindWith'
    , transposeWith
    ) where

import Control.Applicative hiding ((<**>))
import Control.Monad (join)
import Data.Number.Erf
import Data.Traversable (Traversable, mapAccumL)
import Data.Foldable (Foldable, toList)
import Numeric.AD.Internal.Classes
import Numeric.AD.Internal.Identity

data ForwardDouble a = ForwardDouble !Double Double

type instance Scalar (ForwardDouble s) = Double

-- | Calculate the 'tangent' using forward mode AD.
tangent :: ForwardDouble s -> Double
tangent (ForwardDouble _ da) = da
{-# INLINE tangent #-}

unbundle :: ForwardDouble s -> (Double, Double)
unbundle (ForwardDouble a da) = (a, da)
{-# INLINE unbundle #-}

bundle :: Double -> Double -> ForwardDouble s
bundle a da = ForwardDouble a da
{-# INLINE bundle #-}

apply :: (ForwardDouble s -> b) -> Double -> b
apply f a = f (bundle a 1)
{-# INLINE apply #-}

instance Primal (ForwardDouble s) where
    primal (ForwardDouble a _) = a

instance Mode (ForwardDouble s) where
    auto = flip ForwardDouble 0
    zero = ForwardDouble 0 0

    isKnownZero (ForwardDouble 0 0) = True
    isKnownZero _ = False

    isKnownConstant (ForwardDouble _ 0) = True
    isKnownConstant _ = False

    ForwardDouble a da <+> ForwardDouble b db = ForwardDouble (a + b) (da + db)

    x    <**> y      = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log xi)) x y

    a *^ ForwardDouble b db = ForwardDouble (a * b) (a * db)

    ForwardDouble a da ^* b = ForwardDouble (a * b) (da * b)

    ForwardDouble a da ^/ b = ForwardDouble (a / b) (da / b)

instance Jacobian (ForwardDouble s) where
    type D (ForwardDouble s) = Id s Double


    unary f (Id dadb) (ForwardDouble b db) = ForwardDouble (f b) (dadb * db)

    lift1 f df (ForwardDouble b db) = ForwardDouble (f b) (dadb * db)
        where
            Id dadb = df (Id b)

    lift1_ f df (ForwardDouble b db) = ForwardDouble a da
        where
            a = f b
            Id da = df (Id a) (Id b) ^* db

    binary f (Id dadb) (Id dadc) (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble (f b c) $ dadb * db + dc * dadc

    lift2 f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id b) (Id c)
            da = dadb * db + dc * dadc

    lift2_ f df (ForwardDouble b db) (ForwardDouble c dc) = ForwardDouble a da
        where
            a = f b c
            (Id dadb, Id dadc) = df (Id a) (Id b) (Id c)
            da = dadb * db + dc * dadc

instance Eq (ForwardDouble s) where
    (==)          = discrete2 (==)
instance Ord (ForwardDouble s) where
    compare       = discrete2 compare
instance Num (ForwardDouble s) where
    fromInteger 0  = zero
    fromInteger n = auto (fromInteger n)
    (+)          = (<+>) -- binary (+) one one
    (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
    (*)          = lift2 (*) (\x y -> (y, x))
    negate       = lift1 negate (const (auto (-1)))
    abs          = lift1 abs signum
    signum a     = lift1 signum (const zero) a
instance Fractional (ForwardDouble s) where
    fromRational 0 = zero
    fromRational r = auto (fromRational r)
    x / y        = x * recip y
    recip        = lift1_ recip (const . negate . join (*))
instance Floating (ForwardDouble s) where
    pi       = auto pi
    exp      = lift1_ exp const
    log      = lift1 log recip
    logBase x y = log y / log x
    sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
    (**)     = (<**>)
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
instance Enum (ForwardDouble s) where
    succ                 = lift1 succ (const one)
    pred                 = lift1 pred (const one)
    toEnum               = auto . toEnum
    fromEnum             = discrete1 fromEnum
    enumFrom a           = withPrimal a <$> enumFrom (primal a)
    enumFromTo a b       = withPrimal a <$> discrete2 enumFromTo a b
    enumFromThen a b     = zipWith (fromBy a delta) [0..] $ discrete2 enumFromThen a b where delta = b - a
    enumFromThenTo a b c = zipWith (fromBy a delta) [0..] $ discrete3 enumFromThenTo a b c where delta = b - a
instance Real (ForwardDouble s) where
    toRational      = discrete1 toRational
instance RealFloat (ForwardDouble s) where
    floatRadix      = discrete1 floatRadix
    floatDigits     = discrete1 floatDigits
    floatRange      = discrete1 floatRange
    decodeFloat     = discrete1 decodeFloat
    encodeFloat m e = auto (encodeFloat m e)
    isNaN           = discrete1 isNaN
    isInfinite      = discrete1 isInfinite
    isDenormalized  = discrete1 isDenormalized
    isNegativeZero  = discrete1 isNegativeZero
    isIEEE          = discrete1 isIEEE
    exponent = exponent
    scaleFloat n = unary (scaleFloat n) (scaleFloat n one)
    significand x =  unary significand (scaleFloat (- floatDigits x) one) x
    atan2 = lift2 atan2 $ \vx vy -> let r = recip (join (*) vx + join (*) vy) in (vy * r, negate vx * r)
instance (Scalar (D (ForwardDouble s)) ~ Scalar (ForwardDouble s)) => RealFrac (ForwardDouble s) where
    properFraction a = (w, a `withPrimal` pb) where
         pa = primal a
         (w, pb) = properFraction pa
    truncate = discrete1 truncate
    round    = discrete1 round
    ceiling  = discrete1 ceiling
    floor    = discrete1 floor
instance Erf (ForwardDouble s) where
    erf = lift1 erf $ \x -> (fromInteger 2 / sqrt pi) * exp (negate x * x)
    erfc = lift1 erfc $ \x -> (fromInteger (-2) / sqrt pi) * exp (negate x * x)
    normcdf = lift1 normcdf $ \x -> (fromInteger (-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt (fromInteger 2))
instance InvErf (ForwardDouble s) where
    inverf = lift1 inverfc $ \x -> recip $ (fromInteger 2 / sqrt pi) * exp (negate x * x)
    inverfc = lift1 inverfc $ \x -> recip $ negate (fromInteger 2 / sqrt pi) * exp (negate x * x)
    invnormcdf = lift1 invnormcdf $ \x -> recip $ (fromInteger (-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt (fromInteger 2))

bind :: (Traversable f) => (f (ForwardDouble s) -> b) -> f Double -> f b
bind f as = snd $ mapAccumL outer (0 :: Int) as
    where
        outer !i _ = (i + 1, f $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bind' :: (Traversable f) => (f (ForwardDouble s) -> b) -> f Double -> (b, f b)
bind' f as = dropIx $ mapAccumL outer (0 :: Int, b0) as
    where
        outer (!i, _) _ = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), b)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
        b0 = f (auto <$> as)
        dropIx ((_,b),bs) = (b,bs)

bindWith :: (Traversable f) => (Double -> b -> c) -> (f (ForwardDouble s) -> b) -> f Double -> f c
bindWith g f as = snd $ mapAccumL outer (0 :: Int) as
    where
        outer !i a = (i + 1, g a $ f $ snd $ mapAccumL (inner i) 0 as)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)

bindWith' :: (Traversable f) => (Double -> b -> c) -> (f (ForwardDouble s) -> b) -> f Double -> (b, f c)
bindWith' g f as = dropIx $ mapAccumL outer (0 :: Int, b0) as
    where
        outer (!i, _) a = let b = f $ snd $ mapAccumL (inner i) (0 :: Int) as in ((i + 1, b), g a b)
        inner !i !j a = (j + 1, if i == j then bundle a 1 else auto a)
        b0 = f (auto <$> as)
        dropIx ((_,b),bs) = (b,bs)

-- we can't transpose arbitrary traversables, since we can't construct one out of whole cloth, and the outer
-- traversable could be empty. So instead we use one as a 'skeleton'
transposeWith :: (Functor f, Foldable f, Traversable g) => (b -> f a -> c) -> f (g a) -> g b -> g c
transposeWith f as = snd . mapAccumL go xss0
    where
        go xss b = (tail <$> xss, f b (head <$> xss))
        xss0 = toList <$> as

