{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, FlexibleContexts, TypeFamilies #-}
module Numeric.Rounding 
    ( Round(..)
    , Rounding
    , Precision
    , C
    , Up, Down, Trunc, ToNearest
    , up, down, trunc
    , runUp, runDown, runTrunc
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
    deriving (Show, Read, Eq, Ord, Bounded)

{-# RULES
"realToFrac/Round->a" realToFrac = \(Round x) -> x
"realToFrac/a->Round" realToFrac = Round
  #-}

instance Functor (Round dir) where
    fmap f (Round a) = Round (f a)

instance Foldable (Round dir) where
    foldMap f (Round a) = f a

instance Traversable (Round dir) where
    traverse f (Round a) = Round <$> f a

class Rounding dir where
    mode :: Round dir a -> CInt
    rounding :: (Integral b, RealFrac a) => Round dir c -> (a -> b)
    pi_m :: Precision a => Ops a -> Round dir a

data ToNearest
data Trunc
data Up
data Down

instance Rounding ToNearest where 
    mode _ = #const FE_TONEAREST
    rounding _ = round
    pi_m _ = pi

instance Rounding Trunc where 
    mode _ = #const FE_TOWARDZERO
    rounding _ = truncate
    pi_m = Round . realToFrac . unsafePerformIO . peek . pi_l

instance Rounding Up where 
    mode _ = #const FE_UPWARD
    rounding _ = ceiling
    pi_m = Round . realToFrac . unsafePerformIO . peek . pi_u

instance Rounding Down where 
    mode _ = #const FE_DOWNWARD
    rounding _ = floor
    pi_m = Round . realToFrac . unsafePerformIO . peek . pi_l

type U a = CInt -> C a -> C a 
type B a = CInt -> C a -> C a -> C a 

class (Storable (C a), RealFloat (C a), RealFloat a, Enum a) => Precision a where
    type C a :: *
    ops :: Round m a -> Ops a
    lift1 :: Rounding m => (Ops a -> U a) -> Round m a -> Round m a 
    lift2 :: Rounding m => (Ops a -> B a) -> Round m a -> Round m a -> Round m a

data Ops a = Ops 
    { pi_l :: Ptr (C a) 
    , pi_u :: Ptr (C a)
    , padd :: B a
    , pminus :: B a 
    , ptimes :: B a
    , pdiv :: B a
    , pexp :: U a 
    , ppow :: B a
    , plog :: U a 
    , psqrt :: U a 
    , psin :: U a 
    , pcos :: U a 
    , ptan :: U a
    , psinh :: U a
    , pcosh :: U a
    , ptanh :: U a
    , pasin :: U a
    , pacos :: U a
    , patan :: U a
    , pasinh :: U a
    , pacosh :: U a 
    , patanh :: U a
    , patan2 :: B a
--  , pfmod :: B a 
    }

instance Precision Double where
    type C Double = CDouble
    lift1 f r@(Round x) = Round (realToFrac (f (ops r) (mode r) (realToFrac x)))
    lift2 f r@(Round x) (Round y) = Round (realToFrac (f (ops r) (mode r) (realToFrac x) (realToFrac y)))
    ops _ = Ops 
        { pi_l = pi_d_l
        , pi_u = pi_d_u
        , padd = madd
        , pminus = mminus
        , ptimes = mtimes
        , pdiv = mdiv
        , pexp = mexp
        , ppow = mpow
        , plog = mlog
        , psqrt = msqrt
        , psin = msin
        , pcos = mcos
        , ptan = mtan
        , psinh = msinh
        , pcosh = mcosh
        , ptanh = mtanh
        , pasin = masin
        , pacos = macos
        , patan = matan
        , pasinh = masinh
        , pacosh = macosh
        , patanh = matanh
        , patan2 = matan2
      --, pfmod = mfmod
        }

instance Precision Float where
    type C Float = CFloat
    lift1 f r@(Round x) = Round (realToFrac (f (ops r) (mode r) (realToFrac x)))
    lift2 f r@(Round x) (Round y) = Round (realToFrac (f (ops r) (mode r) (realToFrac x) (realToFrac y)))
    ops _ = Ops 
        { pi_l = pi_f_l
        , pi_u = pi_f_u
        , padd = maddf
        , pminus = mminusf
        , ptimes = mtimesf
        , pdiv = mdivf
        , pexp = mexpf
        , ppow = mpowf
        , plog = mlogf
        , psqrt = msqrtf
        , psin = msinf
        , pcos = mcosf
        , ptan = mtanf
        , psinh = msinhf
        , pcosh = mcoshf
        , ptanh = mtanhf
        , pasin = masinf
        , pacos = macosf
        , patan = matanf
        , pasinh = masinhf
        , pacosh = macoshf
        , patanh = matanhf
        , patan2 = matan2f
      --, pfmod = mfmodf
        }

instance (Rounding d, Precision a) => Num (Round d a) where
    fromInteger n = Round (fromInteger n)
    (+) = lift2 padd
    (-) = lift2 pminus
    (*) = lift2 ptimes
    abs (Round a) = Round (abs a)
    signum (Round a) = Round (signum a)

instance (Rounding d, Precision a) => Fractional (Round d a) where
    (/) = lift2 pdiv
    recip = lift2 pdiv 1
    fromRational = fromRat
    
instance (Rounding d, Precision a) => Enum (Round d a) where
    succ = (+1)
    pred = subtract 1
    toEnum n = Round (toEnum n) -- TODO: tweak?
    fromEnum (Round a) = fromEnum a -- TODO: tweak?
    enumFrom = numericEnumFrom
    enumFromThen = numericEnumFromThen
    enumFromTo = numericEnumFromTo
    enumFromThenTo = numericEnumFromThenTo
    
instance (Rounding d, Precision a) => Floating (Round d a) where
    pi    = r where r = pi_m (ops r)
    exp   = lift1 pexp
    (**)  = lift2 ppow
    log   = lift1 plog
    sqrt  = lift1 psqrt
    sin   = lift1 psin
    cos   = lift1 pcos
    tan   = lift1 ptan
    asin  = lift1 pasin
    acos  = lift1 pacos
    atan  = lift1 patan
    sinh  = lift1 psinh
    cosh  = lift1 pcosh
    tanh  = lift1 ptanh
    asinh = lift1 pasinh
    acosh = lift1 pacosh
    atanh = lift1 patanh

instance (Rounding d, Precision a) => Real (Round d a) where
    toRational (Round a) = toRational a -- tweak?

instance (Rounding d, Precision a) => RealFrac (Round d a) where
    properFraction = properFrac
    truncate (Round a) = truncate a
    round (Round a) = round a
    ceiling (Round a) = ceiling a
    floor (Round a) = floor a

instance (Rounding d, Precision a) => RealFloat (Round d a) where
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
    atan2 = lift2 patan2

-- * Fractional 

properFrac 
    :: (Rounding dir, RealFrac a, Integral b) => 
       Round dir a -> (b, Round dir a)
properFrac (Round a) = (b, Round c) 
    where
    (b, c) = properFraction a

-- * Rounding Rationals

fromRat :: (Rounding d, Precision a) => Rational -> Round d a
fromRat (n :% 0) = case compare n 0 of
    GT -> 1/0 -- +Infinity
    EQ -> 0/0 -- NaN
    LT -> -1/0 -- -Infinity
fromRat (n :% d) = case compare n 0 of
    GT -> fromRat' (n :% d)
    EQ -> encodeFloat 0 0 -- Zero
    LT -> - fromRat' ((-n) :% d)


-- Conversion process:
-- Scale the rational number by the RealFloat base until
-- it lies in the range of the mantissa (as used by decodeFloat/encodeFloat).
-- Then round the rational to an Integer and encode it with the exponent
-- that we got from the scaling.
-- To speed up the scaling process we compute the log2 of the number to get
-- a first guess of the exponent.

fromRat' :: (Rounding d, Precision a) => Rational -> Round d a
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

trunc :: a -> Round Trunc a
trunc = Round
{-# INLINE trunc #-}

runUp :: Round Up a -> a 
runUp (Round a) = a
{-# INLINE runUp #-}

runDown :: Round Down a -> a
runDown (Round a) = a
{-# INLINE runDown #-}

runTrunc :: Round Trunc a -> a
runTrunc (Round a) = a
{-# INLINE runTrunc #-}


foreign import ccall "rounding.h &pi_d_l"
    pi_d_l :: Ptr CDouble
foreign import ccall "rounding.h &pi_d_u"
    pi_d_u :: Ptr CDouble
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

foreign import ccall "rounding.h &pi_f_l"
    pi_f_l :: Ptr CFloat
foreign import ccall "rounding.h &pi_f_u"
    pi_f_u :: Ptr CFloat
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
