{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Composition
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Composition
    ( ComposeFunctor(..)
    , ComposeMode(..)
    ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Applicative hiding ((<**>))
import Data.Number.Erf
import Data.Data
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
import Numeric.AD.Internal.Classes

#ifdef HLINT
{-# ANN module "Hlint: ignore Eta reduce" #-}
{-# ANN module "Hlint: ignore Reduce duplication" #-}
#endif

------------------------------------------------------------------------------
-- ComposeMode
------------------------------------------------------------------------------

-- | The composition of two AD modes is an AD mode in its own right
newtype ComposeMode f a = ComposeMode { decomposeMode :: f a }
--  deriving (Enum, Eq, Ord, Bounded, Num, Fractional, Floating, RealFloat, RealFrac, Real, Erf, InvErf)

--deriving instance RealFloat (f a)=> RealFloat (ComposeMode f a)
--deriving instance RealFrac (f a) => RealFrac (ComposeMode f a)
--deriving instance Real (f a) => Real (ComposeMode f a)
--deriving instance Erf (f a) => Erf (ComposeMode f a)
--deriving instance InvErf (f a) => InvErf (ComposeMode f a)

type instance Scalar (ComposeMode f a) = Scalar a

instance (Lifted (f a), Eq a, Num a, Scalar (f a) ~ a) => Eq (ComposeMode f a) where
  ComposeMode a == ComposeMode b = liftEq [a] $ a == b

instance (Lifted (f a), Enum a, Num a, Scalar (f a) ~ a) => Enum (ComposeMode f a) where
  succ (ComposeMode a) = ComposeMode $ liftedEnum (succ a)
  pred (ComposeMode a) = ComposeMode $ liftedEnum (pred a)
  fromEnum (ComposeMode a) = liftEnum [a] (fromEnum a)
  toEnum i = ComposeMode $ liftedEnum $ toEnum i
  enumFrom (ComposeMode a) = ComposeMode <$> liftEnum [a] (enumFrom a)
  enumFromTo (ComposeMode a) (ComposeMode b) = ComposeMode <$> liftEnum [a] (enumFromTo a b)
  enumFromThen (ComposeMode a) (ComposeMode b) = ComposeMode <$> liftEnum [a] (enumFromThen a b)
  enumFromThenTo (ComposeMode a) (ComposeMode b) (ComposeMode c) = ComposeMode <$> liftEnum [a] (enumFromThenTo a b c)

instance (Lifted (f a), Ord a, Num a, Scalar (f a) ~ a) => Ord (ComposeMode f a) where
  compare (ComposeMode a) (ComposeMode b) = liftOrd [a] $ compare a b

instance (Lifted (f a), Bounded a, Num a, Scalar (f a) ~ a) => Bounded (ComposeMode f a) where
  minBound = ComposeMode $ liftedBounded minBound
  maxBound = ComposeMode $ liftedBounded minBound

instance (Lifted (f a), Num a, Scalar (f a) ~ a) => Num (ComposeMode f a) where
  ComposeMode a + ComposeMode b = ComposeMode $ liftedNum (a + b)
  ComposeMode a - ComposeMode b = ComposeMode $ liftedNum (a - b)
  ComposeMode a * ComposeMode b = ComposeMode $ liftedNum (a * b)
  abs (ComposeMode a) = ComposeMode $ liftedNum (abs a)
  signum (ComposeMode a) = ComposeMode $ liftedNum (signum a)
  fromInteger i = ComposeMode $ liftedNum $ fromInteger i

instance (Lifted (f a), Real a, Scalar (f a) ~ a) => Real (ComposeMode f a) where
  toRational (ComposeMode a) = liftReal [a] $ toRational a

instance (Lifted (f a), Fractional a, Scalar (f a) ~ a) => Fractional (ComposeMode f a) where
  ComposeMode a / ComposeMode b = ComposeMode $ liftedFractional (a / b)
  recip (ComposeMode a) = ComposeMode $ liftedFractional (recip a)
  fromRational r = ComposeMode $ liftedFractional $ fromRational r

instance (Lifted (f a), RealFrac a, Scalar (f a) ~ a) => RealFrac (ComposeMode f a) where
  properFraction (ComposeMode a) = liftRealFrac [a] $ case properFraction a of
    (i, b) -> (i, ComposeMode b)
  truncate (ComposeMode a) = liftRealFrac [a] (truncate a)
  round    (ComposeMode a) = liftRealFrac [a] (round a)
  ceiling  (ComposeMode a) = liftRealFrac [a] (ceiling a)
  floor    (ComposeMode a) = liftRealFrac [a] (floor a)

instance (Lifted (f a), Floating a, Scalar (f a) ~ a) => Floating (ComposeMode f a) where
  pi = ComposeMode $ liftedFloating pi
  exp (ComposeMode a) = ComposeMode $ liftedFloating (exp a)
  sqrt (ComposeMode a) = ComposeMode $ liftedFloating (sqrt a)
  log (ComposeMode a) = ComposeMode $ liftedFloating (log a)
  ComposeMode a ** ComposeMode b = ComposeMode $ liftedFloating (a ** b)
  logBase (ComposeMode a) (ComposeMode b) = ComposeMode $ liftedFloating (logBase a b)
  sin (ComposeMode a) = ComposeMode $ liftedFloating (sin a)
  tan (ComposeMode a) = ComposeMode $ liftedFloating (tan a)
  cos (ComposeMode a) = ComposeMode $ liftedFloating (cos a)
  asin (ComposeMode a) = ComposeMode $ liftedFloating (asin a)
  atan (ComposeMode a) = ComposeMode $ liftedFloating (atan a)
  acos (ComposeMode a) = ComposeMode $ liftedFloating (acos a)
  sinh (ComposeMode a) = ComposeMode $ liftedFloating (sinh a)
  tanh (ComposeMode a) = ComposeMode $ liftedFloating (tanh a)
  cosh (ComposeMode a) = ComposeMode $ liftedFloating (cosh a)
  asinh (ComposeMode a) = ComposeMode $ liftedFloating (asinh a)
  atanh (ComposeMode a) = ComposeMode $ liftedFloating (atanh a)
  acosh (ComposeMode a) = ComposeMode $ liftedFloating (acosh a)

instance (Lifted (f a), Erf a, Scalar (f a) ~ a) => Erf (ComposeMode f a) where
  erf (ComposeMode a) = ComposeMode $ liftedErf (erf a)
  erfc (ComposeMode a) = ComposeMode $ liftedErf (erfc a)

instance (Lifted (f a), InvErf a, Scalar (f a) ~ a) => InvErf (ComposeMode f a) where
  inverf (ComposeMode a) = ComposeMode $ liftedInvErf (inverf a)
  inverfc (ComposeMode a) = ComposeMode $ liftedInvErf (inverfc a)
  invnormcdf (ComposeMode a) = ComposeMode $ liftedInvErf (invnormcdf a)

instance (Lifted (f a), Primal a, Num a, Scalar (f a) ~ a) => Primal (ComposeMode f a) where
  primal (ComposeMode a) = liftPrimal [a] $ primal $ primal a

instance (Lifted (f a), RealFloat a, Scalar (f a) ~ a) => RealFloat (ComposeMode f a) where
  floatRadix (ComposeMode a) = liftRealFloat [a] (floatRadix a)
  floatDigits (ComposeMode a) = liftRealFloat [a] (floatDigits a)
  floatRange (ComposeMode a) = liftRealFloat [a] (floatRange a)
  decodeFloat (ComposeMode a) = liftRealFloat [a] (decodeFloat a)
  encodeFloat i j = ComposeMode $ liftedRealFloat (encodeFloat i j)
  exponent (ComposeMode a) = liftRealFloat [a] (exponent a)
  significand (ComposeMode a) = ComposeMode $ liftedRealFloat (significand a)
  scaleFloat n (ComposeMode a) = ComposeMode $ liftedRealFloat (scaleFloat n a)
  isNaN (ComposeMode a) = liftRealFloat [a] (isNaN a)
  isInfinite (ComposeMode a) = liftRealFloat [a] (isInfinite a)
  isDenormalized (ComposeMode a) = liftRealFloat [a] (isDenormalized a)
  isNegativeZero (ComposeMode a) = liftRealFloat [a] (isNegativeZero a)
  isIEEE (ComposeMode a) = liftRealFloat [a] (isIEEE a)
  atan2 (ComposeMode a) (ComposeMode b) = ComposeMode $ liftedRealFloat (atan2 a b)


instance (Lifted (f a), Lifted a, Mode a, Num a, Scalar (f a) ~ a, Num (Scalar a)) => Mode (ComposeMode f a) where
  auto a = ComposeMode $ liftedMode $ auto (auto a)
  ComposeMode a <+> ComposeMode b = ComposeMode $ liftedMode (a <+> b)
  a *^ ComposeMode b = ComposeMode $ liftedMode (auto a *^ b)
  ComposeMode a ^* b = ComposeMode $ liftedMode (a ^* auto b)
  -- ComposeMode a ^/ b = ComposeMode $ liftedMode (a ^/ auto b)
  ComposeMode a <**> ComposeMode b = ComposeMode $ liftedMode $ liftedFloating' $ a <**> b

danger :: p (ComposeMode f a) -> p a
danger = undefined

instance (Lifted (f a), Lifted a, a ~ Scalar (f a), Num (Scalar a), Num a) => Lifted (ComposeMode f a) where
    liftBounded    p a = liftBounded (danger p) a
    liftEnum       p a = liftEnum (danger p) a
    liftEq         p a = liftEq (danger p) a
    liftOrd        p a = liftOrd (danger p) a
    liftNum        p a = liftNum (danger p) a
    liftFractional p a = liftFractional (danger p) a
    liftFloating   p a = liftFloating (danger p) a
    liftRealFloat  p a = liftRealFloat (danger p) a
    liftRealFrac   p a = liftRealFrac (danger p) a
    liftReal       p a = liftReal (danger p) a
    liftErf        p a = liftErf (danger p) a
    liftInvErf     p a = liftInvErf (danger p) a
    liftMode       p a = liftMode (danger p) a
    liftPrimal     p a = liftPrimal (danger p) a

{-
instance (Primal (f a), Primal a, a ~ Scalar (f a)) => Primal (ComposeMode f a) where
  primal = primal . primal . decomposeMode

instance (Mode (f a), Mode a, a ~ Scalar (f a)) => Mode (ComposeMode f a) where
    auto = ComposeMode . auto . auto
    ComposeMode a <+> ComposeMode b = ComposeMode (a <+> b)
    a *^ ComposeMode b = ComposeMode (auto a *^ b)
    ComposeMode a ^* b = ComposeMode (a ^* auto b)
    ComposeMode a ^/ b = ComposeMode (a ^/ auto b)
    ComposeMode a <**> ComposeMode b = ComposeMode (a <**> b)
-}
{-
instance (Mode (f (g a s)) s', Mode (g a) s, Scalar (f (g a s) s') ~ g a s, Scalar (g a s) ~ a, Floating (g a s)) => Mode (ComposeMode f g a s) s' where

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable ComposeMode
deriving instance (Typeable f, Typeable g, Typeable s, Data (f (g a s) s'), Data a) => Data (ComposeMode f g a s s')
#else
instance (Typeable2 f, Typeable2 g) => Typeable3 (ComposeMode f g) where
    typeOf3 tfg = mkTyConApp composeModeTyCon [typeOf2 (fa tfg), typeOf2 (ga tfg)]
        where fa :: t f (g :: * -> * -> *) a s s' -> f a s'
              fa = undefined
              ga :: t (f :: * -> * -> *) g a s s'-> g a s
              ga = undefined

instance (Typeable2 f, Typeable2 g, Typeable a, Typeable s, Typeable s') => Typeable (ComposeMode f g a s s') where
    typeOf = typeOfDefault

composeModeTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
composeModeTyCon = mkTyCon3 "ad" "Numeric.AD.Internal.Composition" "ComposeMode"
#else
composeModeTyCon = mkTyCon "Numeric.AD.Internal.Composition.ComposeMode"
#endif
{-# NOINLINE composeModeTyCon #-}

composeModeConstr :: Constr
composeModeConstr = mkConstr composeModeDataType "ComposeMode" [] Prefix
{-# NOINLINE composeModeConstr #-}

composeModeDataType :: DataType
composeModeDataType = mkDataType "Numeric.AD.Internal.Composition.ComposeMode" [composeModeConstr]
{-# NOINLINE composeModeDataType #-}

instance (Typeable2 f, Typeable2 g, Data (f (g a s) s'), Data a, Typeable s', Typeable s, Data s') => Data (ComposeMode f g a s s') where
    gfoldl f z (ComposeMode a) = z ComposeMode `f` a
    toConstr _ = composeModeConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z ComposeMode)
        _ -> error "gunfold"
    dataTypeOf _ = composeModeDataType
    dataCast1 f = gcast1 f
#endif

-}

------------------------------------------------------------------------------
-- ComposeFunctor
------------------------------------------------------------------------------

-- | Functor composition, used to nest the use of jacobian and grad
newtype ComposeFunctor f g a = ComposeFunctor { decomposeFunctor :: f (g a) }

instance (Functor f, Functor g) => Functor (ComposeFunctor f g) where
    fmap f (ComposeFunctor a) = ComposeFunctor (fmap (fmap f) a)

instance (Foldable f, Foldable g) => Foldable (ComposeFunctor f g) where
    foldMap f (ComposeFunctor a) = foldMap (foldMap f) a

instance (Traversable f, Traversable g) => Traversable (ComposeFunctor f g) where
    traverse f (ComposeFunctor a) = ComposeFunctor <$> traverse (traverse f) a

#if __GLASGOW_HASKELL__ >= 707
deriving instance Typeable ComposeFunctor
deriving instance (Typeable f, Typeable a, Typeable g, Data (f (g a))) => Data (ComposeFunctor f g a)
#else
instance (Typeable1 f, Typeable1 g) => Typeable1 (ComposeFunctor f g) where
    typeOf1 tfga = mkTyConApp composeFunctorTyCon [typeOf1 (fa tfga), typeOf1 (ga tfga)]
        where fa :: t f (g :: * -> *) a -> f a
              fa = undefined
              ga :: t (f :: * -> *) g a -> g a
              ga = undefined

composeFunctorTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
composeFunctorTyCon = mkTyCon3 "ad" "Numeric.AD.Internal.Composition" "ComposeFunctor"
#else
composeFunctorTyCon = mkTyCon "Numeric.AD.Internal.Composition.ComposeFunctor"
#endif

{-# NOINLINE composeFunctorTyCon #-}

composeFunctorConstr :: Constr
composeFunctorConstr = mkConstr composeFunctorDataType "ComposeFunctor" [] Prefix
{-# NOINLINE composeFunctorConstr #-}

composeFunctorDataType :: DataType
composeFunctorDataType = mkDataType "Numeric.AD.Internal.Composition.ComposeFunctor" [composeFunctorConstr]
{-# NOINLINE composeFunctorDataType #-}

instance (Typeable1 f, Typeable1 g, Data (f (g a)), Data a) => Data (ComposeFunctor f g a) where
    gfoldl f z (ComposeFunctor a) = z ComposeFunctor `f` a
    toConstr _ = composeFunctorConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z ComposeFunctor)
        _ -> error "gunfold"
    dataTypeOf _ = composeFunctorDataType
    dataCast1 f = gcast1 f
#endif

