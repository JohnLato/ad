{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Types
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------
module Numeric.AD.Internal.Types
    ( AD(..)
    ) where

import Numeric.AD.Internal.Classes

-- | 'AD' serves as a common wrapper for different 'Mode' instances, exposing a traditional
-- numerical tower. Universal quantification is used to limit the actions in user code to
-- machinery that will return the same answers under all AD modes, allowing us to use modes
-- interchangeably as both the type level \"brand\" and dictionary, providing a common API.
newtype AD t = AD {runAD :: t} deriving (Iso t, Lifted, Primal)

type instance Scalar (AD t) = Scalar t

deriving instance (Mode t) => Mode (AD t)

instance (Lifted t, Num (Scalar t)) => Num (AD t) where
    AD a + AD b = AD $ liftedNum $ a + b
    AD a * AD b = AD $ liftedNum $ a * b
    AD a - AD b = AD $ liftedNum $ a - b
    negate (AD a) = AD $ liftedNum $ negate a
    abs (AD a) = AD $ liftedNum $ abs a
    signum (AD a) = AD $ liftedNum $ signum a
    fromInteger i = AD $ liftedNum $ fromInteger i

instance (Lifted t, Fractional (Scalar t)) => Fractional (AD t) where
    AD a / AD b = AD $ liftedFractional $ a / b
    recip (AD a) = AD $ liftedFractional $ recip a
    fromRational r = AD $ liftedFractional $ fromRational r

instance (Lifted t, Floating (Scalar t)) => Floating (AD t) where
    pi = AD $ liftedFloating pi
    exp (AD a) = AD $ liftedFloating $ exp a
    sqrt (AD a) = AD $ liftedFloating $ sqrt a
    log (AD a) = AD $ liftedFloating $ log a
    AD a ** AD b = AD $ liftedFloating $ a ** b
    logBase (AD a) (AD b) = AD $ liftedFloating $ logBase a b
    sin (AD a) = AD $ liftedFloating $ sin a
    tan (AD a) = AD $ liftedFloating $ tan a
    cos (AD a) = AD $ liftedFloating $ cos a
    asin (AD a) = AD $ liftedFloating $ asin a
    atan (AD a) = AD $ liftedFloating $ atan a
    acos (AD a) = AD $ liftedFloating $ acos a
    sinh (AD a) = AD $ liftedFloating $ sinh a
    tanh (AD a) = AD $ liftedFloating $ tanh a
    cosh (AD a) = AD $ liftedFloating $ cosh a
    asinh (AD a) = AD $ liftedFloating $ asinh a
    atanh (AD a) = AD $ liftedFloating $ atanh a
    acosh (AD a) = AD $ liftedFloating $ acosh a
