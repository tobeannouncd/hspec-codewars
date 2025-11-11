{-# LANGUAGE ImplicitParams #-}

module Data.Approx (Approx (..), (~=), (/~=), Approx1(..), Approx2(..)) where

import Data.Ratio (Ratio)
import Data.Functor.Identity (Identity (..))
import Data.Complex (Complex(..), magnitude)

infix 4 ~=, /~=

-- | Class for datatypes that have approximate equality.
class Approx a where
  -- | @isApprox epsilon a b@ checks for approximate equality of @a@ and @b@
  --   within a given margin of error @epsilon@.
  --
  --   For numeric types, @epsilon@ should be non-negative. This precondition is
  --   not checked for the pre-defined instances.
  isApprox :: a -> a -> a -> Bool

-- | Infix alias for 'isApprox'. The margin of error is specified with the
--   implicit parameter @epsilon@.
(~=) :: (Approx a, ?epsilon :: a) => a -> a -> Bool
a ~= b = isApprox ?epsilon a b

-- | @x /~= y@ is equivalent to @not (x ~= y)@.
(/~=) :: (Approx a, ?epsilon :: a) => a -> a -> Bool
a /~= b = not (a ~= b)

-- | 'isApprox' restricted to instances of 'RealFloat'. Special values are
--   handled per IEEE 754 rules: @NaN@ is not approximately equal to any other
--   value, and infinite values are only approximately equal to themselves.
isApproxFloating :: (RealFloat a) => a -> a -> a -> Bool
isApproxFloating eps a b
  | isInfinite a || isInfinite b = a == b
  | otherwise = abs (a - b) <= eps * max (abs a) (abs b)

instance Approx Float where
  isApprox = isApproxFloating

instance Approx Double where
  isApprox = isApproxFloating

instance (Approx a, Approx b) => Approx (a,b) where
  isApprox = uncurry $ liftApprox2 isApprox isApprox

instance (Approx a) => Approx (Identity a) where
  isApprox = liftApprox isApprox . runIdentity

instance (Approx a, RealFloat a) => Approx (Complex a) where
  isApprox = liftApprox isApprox . magnitude

-- | Lifting of the 'Approx' class to unary type constructors.
class Approx1 t where
  -- | @liftApprox approx eps@ lifts approximate equality as determined by
  --   @approx@ into the lifted type with a margin of error of @eps@.
  liftApprox :: (a -> a -> a -> Bool) -> a -> t a -> t a -> Bool

instance Approx1 [] where
  liftApprox approx eps = go
   where
    go []     []     = True
    go (x:xs) (y:ys) = approx eps x y && go xs ys
    go _      _      = False

instance Approx1 Maybe where
  liftApprox approx eps ma mb =
    case (ma, mb) of
      (Nothing, Nothing) -> True
      (Just a,   Just b) -> approx eps a b
      _                  -> False

instance Approx1 Identity where
  liftApprox approx eps (Identity a) (Identity b) = approx eps a b

instance Approx1 Complex where
  liftApprox approx eps (a :+ x) (b :+ y) = approx eps a b && approx eps x y

-- | Lifting of the 'Approx' class to binary type constructors.
class Approx2 t where
  -- | @liftApprox2 app1 app2 eps1 eps2@ lifts @app1@ and @app2@ into the lifted
  --   type with the associated margins of error @eps1@ and @eps2@.
  liftApprox2 :: (a -> a -> a -> Bool) -> (b -> b -> b -> Bool) -> a -> b -> t a b -> t a b -> Bool

instance Approx2 Either where
  liftApprox2 approxA approxB epsA epsB eax eby =
    case (eax, eby) of
      (Left a,   Left b) -> approxA epsA a b
      (Right x, Right y) -> approxB epsB x y
      _                  -> False

instance Approx2 (,) where
  liftApprox2 approxA approxB epsA epsB (a,x) (b,y) =
       approxA epsA a b
    && approxB epsB x y
