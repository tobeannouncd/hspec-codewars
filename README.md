# Test.Hspec.Codewars

Utility functions for testing on Codewars with Hspec.

### Blacklisting

#### `Hidden`

```haskell
data Hidden
  -- | Module to be hidden
  = Module {moduleName :: String}
  -- | Symbol from a module to be hidden
  | FromModule {moduleName :: String, symbolName :: String}
```

#### `solutionShouldHide`

```haskell
solutionShouldHide :: Hidden -> Expectation
```

Check that solution hides a module or a symbol from a module.

```haskell
solutionShouldHide $ FromModule "Prelude" "head"
```

#### `solutionShouldHideAll`

```haskell
solutionShouldHideAll :: [Hidden] -> Expectation
```

Check that solution hides all of given modules and symbols.

```haskell
solutionShouldHideAll [FromModule "Prelude" "head", Module "Data.Set"]
```

### Approximate Equality

#### `shouldBeApprox`

```haskell
shouldBeApprox :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
```

Predefined approximately equal expectation with error margin `1e-6`.

```haskell
sqrt 2.0 `shouldBeApprox` (1.4142135 :: Double)
```

#### `shouldBeApproxPrec`

```haskell
shouldBeApproxPrec :: (Fractional a, Ord a, Show a) => a -> a -> a -> Expectation
```

Create approximately equal expectation with margin.

```haskell
shouldBeApprox' = shouldBeApproxPrec 1e-9
```

#### Lifted Approximate Equality

##### `shouldBeApprox1`/`shouldBeApproxPrec1`

```haskell
shouldBeApprox1 :: (Fractional a, Ord a, Show a, Show (t a), Eq1 t) => t a -> t a -> Expectation
shouldBeApproxPrec1 :: (Num a, Ord a, Show a, Show (t a), Eq1 t) => a -> t a -> t a -> Expectation
```

Similar to `shouldBeApprox`/`shouldBeApproxPrec` with equality lifted via `Eq1`.

##### `shouldBeApprox2`/`shouldBeApproxPrec2`

```haskell
shouldBeApprox2 :: (Fractional a, Ord a, Fractional b, Ord b, Show a, Show b, Show (t a b), Eq2 t) => t a b -> t a b -> Expectation
shouldBeApproxPrec2 :: (Num a, Ord a, Num b, Ord b, Show a, Show b, Show (t a b), Eq2 t) => a -> b -> t a b -> t a b -> Expectation
```

Similar to `shouldBeApprox`/`shouldBeApproxPrec` with equality lifted via `Eq2`. Note that two margins of error are required.

##### `shouldBeApprox2'`/`shouldBeApproxPrec2'`

```haskell
shouldBeApprox2' :: (Fractional a, Show a, Show (t a a), Ord a, Eq2 t) => t a a -> t a a -> Expectation
shouldBeApproxPrec2' :: (Show a, Show (t a a), Num a, Ord a, Eq2 t) => a -> t a a -> t a a -> Expectation
```

Similar to `shouldBeApprox2`/`shouldBeApproxPrec2` where both margins of error are of identical type and value.