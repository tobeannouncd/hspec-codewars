module Data.Approx (Approx (..)) where
import Data.Ratio (Ratio)

infix 1 `isApprox`

-- | Class for data that can be compared for equality within some margin.
class Approx a where
  -- | @isApproxWithin m a b@ determines if @a@ is approximately equal to @b@
  --   within the given margin @m@.
  isApproxWithin :: a -> a -> a -> Bool
  -- | The default margin value used for 'isApprox'.
  defaultMargin :: a
  -- | Alias for @'isApproxWithin' 'defaultMargin'@.
  isApprox :: a -> a -> Bool
  isApprox = isApproxWithin defaultMargin
  {-# MINIMAL isApproxWithin, defaultMargin #-}

-- | Lifting of the 'Approx' class to unary type constructors.
class Approx1 t where
  liftApprox :: (a -> a -> a -> Bool) -> a -> t a -> t a -> Bool

-- | Lifting of the 'Approx' class to binary type constructors.
class Approx2 t where
  liftApprox2 :: (a -> a -> a -> Bool) -> (b -> b -> b -> Bool) -> a -> b -> t a b -> t a b -> Bool

isApproxNum :: (Num a, Ord a) => a -> a -> a -> Bool
isApproxNum margin m n = abs (m - n) < abs margin * max 1 (min (abs m) (abs n))

instance Approx Float where
  isApproxWithin = isApproxNum
  defaultMargin = 1e-6

instance Approx Double where
  isApproxWithin = isApproxNum
  defaultMargin = 1e-6

instance (Integral a) => Approx (Ratio a) where
  isApproxWithin = isApproxNum
  defaultMargin = 1e-6

instance (Approx a,Approx b) => Approx (a, b) where
  isApproxWithin (ma,mb) = liftApprox2 isApproxWithin isApproxWithin ma mb
  defaultMargin = (defaultMargin, defaultMargin)

instance Approx1 [] where
  liftApprox f m = go
    where
      go (x:xs) (y:ys) = f m x y && go xs ys
      go []     []     = True
      go _      _      = False

instance Approx1 Maybe where
  liftApprox f m (Just x) (Just y) = f m x y
  liftApprox _ _ Nothing  Nothing  = True
  liftApprox _ _ _        _        = False

instance Approx2 Either where
  liftApprox2 f _ m _  (Left x)  (Left y)  = f m  x y
  liftApprox2 _ g _ m' (Right x) (Right y) = g m' x y
  liftApprox2 _ _ _ _  _         _         = False

instance Approx2 (,) where
  liftApprox2 f g m m' (a,x) (b,y) = f m a b && g m' x y