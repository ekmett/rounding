{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, FlexibleContexts #-}
module Numeric.Rounding 
    ( Round(..)
    , Rounding(..)
    , Up, Down, ToNearest, TowardZero
    , up, down, toNearest, towardZero
    , runUp, runDown, runToNearest, runTowardZero
    , double
    , float
    ) where

import Control.Applicative
import GHC.Real
import Data.Foldable
import Data.Traversable
import Data.Array
import Foreign
import Foreign.C.Types

#include <math.h>
#include <fenv.h>

-- TODO: tweak the lsbs of pi
-- modify the enum instance to properly round
-- implement complex numbers

newtype Round dir a = Round a
    deriving (Show, Eq, Ord, Bounded)

instance Functor (Round dir) where
    fmap f (Round a) = Round (f a)

instance Foldable (Round dir) where
    foldMap f (Round a) = f a

instance Traversable (Round dir) where
    traverse f (Round a) = Round <$> f a

class Rounding dir where
    mode :: Round dir a -> CInt
    rounding :: (Integral b, RealFrac a) => Round dir c -> (a -> b)
    pi_f :: Round dir Float
    pi_d :: Round dir Double

data ToNearest
data TowardZero
data Up
data Down

instance Rounding ToNearest where 
    mode _ = #const FE_TONEAREST
    rounding _ = round
    pi_f = Round pi
    pi_d = Round pi

instance Rounding TowardZero where 
    mode _ = #const FE_TOWARDZERO
    rounding _ = truncate
    pi_f = Round (realToFrac pi_f_l)
    pi_d = Round (realToFrac pi_d_l)

instance Rounding Up where 
    mode _ = #const FE_UPWARD
    rounding _ = ceiling
    pi_f = Round (realToFrac pi_f_u)
    pi_d = Round (realToFrac pi_d_u)

instance Rounding Down where 
    mode _ = #const FE_DOWNWARD
    rounding _ = floor
    pi_f = Round (realToFrac pi_f_l)
    pi_d = Round (realToFrac pi_d_l)

-- * Rounded Doubles

lift1D :: Rounding m => 
          (CInt -> CDouble -> CDouble) -> 
          Round m Double -> Round m Double
lift1D f r@(Round x) = Round (realToFrac (f (mode r) (realToFrac x)))

lift2D :: Rounding m => 
          (CInt -> CDouble -> CDouble -> CDouble) -> 
          Round m Double -> Round m Double -> Round m Double
lift2D f r@(Round x) (Round y) = Round (realToFrac (f (mode r) (realToFrac x) (realToFrac y)))

foreign import ccall unsafe "rounding.h pi_d_l"
    pi_d_l :: CDouble
foreign import ccall unsafe "rounding.h pi_d_u"
    pi_d_u :: CDouble
foreign import ccall unsafe "rounding.h madd" 
    madd :: CInt -> CDouble -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mminus" 
    mminus :: CInt -> CDouble -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mtimes"
    mtimes :: CInt -> CDouble -> CDouble -> CDouble 
foreign import ccall unsafe "rounding.h mdiv"
    mdiv :: CInt -> CDouble -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mexp"
    mexp :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mpow"
    mpow :: CInt -> CDouble -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mlog"
    mlog :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h msqrt"
    msqrt :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h msin"
    msin :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mcos"
    mcos :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mtan"
    mtan :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h msinh"
    msinh :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mcosh"
    mcosh :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h mtanh"
    mtanh :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h masin"
    masin :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h macos"
    macos :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h matan"
    matan :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h masinh"
    masinh :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h macosh"
    macosh :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h matanh"
    matanh :: CInt -> CDouble -> CDouble
foreign import ccall unsafe "rounding.h matan2"
    matan2 :: CInt -> CDouble -> CDouble -> CDouble

instance Rounding dir => Num (Round dir Double) where
    fromInteger n = Round (fromInteger n)
    (+) = lift2D madd
    (-) = lift2D mminus
    (*) = lift2D mtimes
    abs (Round a) = Round (abs a)
    signum (Round a) = Round (signum a)

instance Rounding dir => Fractional (Round dir Double) where
    (/) = lift2D mdiv
    recip = lift2D mdiv 1
    fromRational = fromRat
    
instance Rounding dir => Enum (Round dir Double) where
    succ = (+1)
    pred = subtract 1
    toEnum n = Round (toEnum n)
    fromEnum (Round a) = fromEnum a
    enumFrom = numericEnumFrom
    enumFromThen = numericEnumFromThen
    enumFromTo = numericEnumFromTo
    enumFromThenTo = numericEnumFromThenTo
    
instance Rounding dir => Floating (Round dir Double) where
    pi = pi_d
    exp = lift1D mexp
    (**) = lift2D mpow
    log = lift1D mlog
    sqrt = lift1D msqrt
    sin = lift1D msin
    cos = lift1D mcos
    tan = lift1D mtan
    asin = lift1D masin
    acos = lift1D macos
    atan = lift1D matan
    sinh = lift1D msinh
    cosh = lift1D mcosh
    tanh = lift1D mtanh
    asinh = lift1D masinh
    acosh = lift1D macosh
    atanh = lift1D matanh

instance Rounding dir => Real (Round dir Double) where
    toRational (Round a) = toRational a -- tweak?

instance Rounding dir => RealFrac (Round dir Double) where
    properFraction = properFrac
    truncate (Round a) = truncate a
    round (Round a) = round a
    ceiling (Round a) = ceiling a
    floor (Round a) = floor a

instance Rounding dir => RealFloat (Round dir Double) where
    floatRadix (Round a) = floatRadix a
    floatDigits (Round a) = floatDigits a
    floatRange (Round a) = floatRange a
    decodeFloat (Round a) = decodeFloat a
    encodeFloat m e = Round (encodeFloat m e)
    exponent (Round a) = exponent a
    significand (Round a) = Round (significand a)
    scaleFloat n (Round a) = Round (scaleFloat n a)
    isNaN (Round a) = isNaN a
    isInfinite (Round a) = isInfinite a
    isDenormalized (Round a) = isDenormalized a
    isNegativeZero (Round a) = isNegativeZero a
    isIEEE (Round a) = isIEEE a
    atan2 = lift2D matan2

-- * Rounded Floats

lift1F :: Rounding m => 
          (CInt -> CFloat -> CFloat ) -> 
          Round m Float -> Round m Float
lift1F f r@(Round x) = Round (realToFrac (f (mode r) (realToFrac x)))

lift2F :: Rounding m => 
          (CInt -> CFloat -> CFloat -> CFloat) -> 
          Round m Float -> Round m Float -> Round m Float
lift2F f r@(Round x) (Round y) = Round (realToFrac (f (mode r) (realToFrac x) (realToFrac y)))

foreign import ccall unsafe "rounding.h pi_f_l"
    pi_f_l :: CFloat
foreign import ccall unsafe "rounding.h pi_f_u"
    pi_f_u :: CFloat
foreign import ccall unsafe "rounding.h madd" 
    maddf :: CInt -> CFloat -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mminus" 
    mminusf :: CInt -> CFloat -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mtimesf"
    mtimesf :: CInt -> CFloat -> CFloat -> CFloat 
foreign import ccall unsafe "rounding.h mdivf"
    mdivf :: CInt -> CFloat -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mexpf"
    mexpf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mpowf"
    mpowf :: CInt -> CFloat -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mlogf"
    mlogf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h msqrtf"
    msqrtf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h msinf"
    msinf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mcosf"
    mcosf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mtanf"
    mtanf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h msinhf"
    msinhf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mcoshf"
    mcoshf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h mtanhf"
    mtanhf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h masinf"
    masinf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h macosf"
    macosf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h matanf"
    matanf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h masinhf"
    masinhf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h macoshf"
    macoshf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h matanhf"
    matanhf :: CInt -> CFloat -> CFloat
foreign import ccall unsafe "rounding.h matan2f"
    matan2f :: CInt -> CFloat -> CFloat -> CFloat

instance Rounding dir => Num (Round dir Float) where
    fromInteger n = Round (fromInteger n)
    (+) = lift2F maddf
    (-) = lift2F mminusf
    (*) = lift2F mtimesf
    abs (Round a) = Round (abs a)
    signum (Round a) = Round (signum a)

instance Rounding dir => Fractional (Round dir Float) where
    (/) = lift2F mdivf
    recip = lift2F mdivf 1
    fromRational = fromRat
    
instance Rounding dir => Enum (Round dir Float) where
    succ = (+1)
    pred = subtract 1
    toEnum n = Round (toEnum n)
    fromEnum (Round a) = fromEnum a
    enumFrom = numericEnumFrom
    enumFromThen = numericEnumFromThen
    enumFromTo = numericEnumFromTo
    enumFromThenTo = numericEnumFromThenTo
    
instance Rounding dir => Floating (Round dir Float) where
    pi = pi_f
    exp = lift1F mexpf
    (**) = lift2F mpowf
    log = lift1F mlogf
    sqrt = lift1F msqrtf
    sin = lift1F msinf
    cos = lift1F mcosf
    tan = lift1F mtanf
    asin = lift1F masinf
    acos = lift1F macosf
    atan = lift1F matanf
    sinh = lift1F msinhf
    cosh = lift1F mcoshf
    tanh = lift1F mtanhf
    asinh = lift1F masinhf
    acosh = lift1F macoshf
    atanh = lift1F matanhf

instance Rounding dir => Real (Round dir Float) where
    toRational (Round a) = toRational a -- tweak?

instance Rounding dir => RealFrac (Round dir Float) where
    properFraction = properFrac
    truncate (Round a) = truncate a
    round (Round a) = round a
    ceiling (Round a) = ceiling a
    floor (Round a) = floor a

instance Rounding dir => RealFloat (Round dir Float) where
    floatRadix (Round a) = floatRadix a
    floatDigits (Round a) = floatDigits a
    floatRange (Round a) = floatRange a
    decodeFloat (Round a) = decodeFloat a
    encodeFloat m e = Round (encodeFloat m e)
    exponent (Round a) = exponent a
    significand (Round a) = Round (significand a)
    scaleFloat n (Round a) = Round (scaleFloat n a)
    isNaN (Round a) = isNaN a
    isInfinite (Round a) = isInfinite a
    isDenormalized (Round a) = isDenormalized a
    isNegativeZero (Round a) = isNegativeZero a
    isIEEE (Round a) = isIEEE a
    atan2 = lift2F matan2f

-- * Fractional 

properFrac 
    :: (Rounding dir, RealFrac a, Integral b) => 
       Round dir a -> (b, Round dir a)
properFrac (Round a) = (b, Round c) 
    where
    (b, c) = properFraction a

-- * Rounding Rationals

fromRat :: (Rounding dir, RealFloat a, RealFloat (Round dir a)) => Rational -> Round dir a
fromRat (n :% 0) | n > 0  =  1/0 -- +Infinity
                 | n == 0 =  0/0 -- NaN
                 | n < 0  = -1/0 -- -Infinity
fromRat (n :% d) | n > 0  = fromRat' (n :% d)
                 | n == 0 = encodeFloat 0 0 -- Zero
                 | n < 0  = - fromRat' ((-n) :% d)


-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.

fromRat' :: (Rounding dir, RealFloat a, RealFloat (Round dir a)) => Rational -> Round dir a
-- Invariant: argument is strictly positive
fromRat' x = r
  where b = floatRadix r
        p = floatDigits r
        (minExp0, _) = floatRange r
        minExp = minExp0 - p            -- the real minimum exponent
        xMin   = toRational (expt b (p-1))
        xMax   = toRational (expt b p)
        p0 = (integerLogBase b (numerator x) - integerLogBase b (denominator x) - p) `max` minExp
        f = if p0 < 0 then 1 % expt b (-p0) else expt b p0 % 1
        (x', p') = scaleRat (toRational b) minExp xMin xMax p0 (x / f)
        r = encodeFloat (rounding r x') p'

-- Scale x until xMin <= x < xMax, or p (the exponent) <= minExp.
scaleRat :: Rational -> Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)
scaleRat b minExp xMin xMax p x 
 | p <= minExp = (x, p)
 | x >= xMax   = scaleRat b minExp xMin xMax (p+1) (x/b)
 | x < xMin    = scaleRat b minExp xMin xMax (p-1) (x*b)
 | otherwise   = (x, p)


-- Exponentiation with a cache for the most common numbers.
minExpt, maxExpt :: Int
minExpt = 0
maxExpt = 1100

expt :: Integer -> Int -> Integer
expt base n | base == 2 && n >= minExpt && n <= maxExpt = expts ! n
            | otherwise = base^n

expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

-- Compute the (floor of the) log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b, but that would
-- be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i
   | i < b     = 0
   | otherwise = doDiv (i `div` (b^l)) l
       where
        -- Try squaring the base first to cut down the number of divisions.
         l = 2 * integerLogBase (b*b) i

         doDiv :: Integer -> Int -> Int
         doDiv x y
            | x < b     = y
            | otherwise = doDiv (x `div` b) (y+1)

up :: a -> Round Up a 
up = Round
{-# INLINE up #-}

down :: a -> Round Down a
down = Round
{-# INLINE down #-}

towardZero :: a -> Round TowardZero a 
towardZero = Round
{-# INLINE towardZero #-}

toNearest :: a -> Round ToNearest a
toNearest = Round
{-# INLINE toNearest #-}

runUp :: Round Up a -> a
runUp (Round a) = a
{-# INLINE runUp #-}

runDown :: Round Down a -> a
runDown (Round a) = a
{-# INLINE runDown #-}

runTowardZero :: Round TowardZero a -> a
runTowardZero (Round a) = a
{-# INLINE runTowardZero #-}

runToNearest :: Round ToNearest a -> a
runToNearest (Round a) = a
{-# INLINE runToNearest #-}

double :: Double -> Double
double = id
{-# INLINE double #-}

float :: Float -> Float
float = id
{-# INLINE float #-}

