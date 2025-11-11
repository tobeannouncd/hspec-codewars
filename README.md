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
shouldBeApprox :: (Approx a, Show a) => a -> a -> Expectation
```

Predefined approximately equal expectation with error margin `1e-6`.

```haskell
sqrt 2.0 `shouldBeApprox` (1.4142135 :: Double)
```

#### `shouldBeApproxPrec`

```haskell
shouldBeApproxPrec :: (Approx a, Show a) => a -> a -> a -> Expectation
```

Create approximately equal expectation with margin.

```haskell
shouldBeApprox' = shouldBeApproxPrec 1e-9
```

### Utility Functions

#### `shouldBeWith`

```haskell
shouldBeWith :: (Show a) => (a -> a -> Bool) -> a -> a -> Expectation
```

Like `shouldBe`, but with an explicitly given equality function.

The supplied equality function does not have to be symmetric, and shall be
called with the expected value as the second argument.

```haskell
shouldBeWith ((==) `on` sort) "abc" "cba"
```

#### `shouldBeWithShow`

```haskell
shouldBeWithShow :: (a -> a -> Bool) -> (a -> a -> String) -> a -> a -> Expectation
```

Like `shouldBeWith`, but with an explicitly given show function for failed tests.


# Data.Approx

### Approximate Equality

#### `Approx`

Class for data that can be compared for equality within some margin.

##### `isApprox`

```haskell
isApprox :: (Approx a) => a -> a -> a -> Bool
```

`isApprox epsilon a b` checks for approximate equality of `a` and `b` within a given margin of error `epsilon`.

For numeric types, `epsilon` should be non-negative. This precondition is not checked for the pre-defined instances.

##### `(~=)`

```haskell
(~=) :: (Approx a, ?epsilon :: a) => a -> a -> Bool
```

Infix alias for `isApprox`. The margin of error is specified with the implicit parameter `epsilon`.

##### `(/~=)`

```haskell
(/~=) :: (Approx a, ?epsilon :: a) => a -> a -> Bool
```

`x /~= y` is equivalent to `not (x ~= y)`.

#### `Approx1`

Lifting of the `Approx` class to unary type constructors.

##### `liftApprox`

```haskell
liftApprox :: (Approx1 t) => (a -> a -> a -> Bool) -> a -> t a -> t a -> Bool
```

`liftApprox approx eps` lifts approximate equality as determined by `approx` into the lifted type with a margin of error of `eps`.


#### `Approx2`

Lifting of the `Approx` class to binary type constructors.

##### `liftApprox2`

```haskell
liftApprox2 :: (Approx2 t) => (a -> a -> a -> Bool) -> (b -> b -> b -> Bool) -> a -> b -> t a b -> t a b -> Bool
```

`liftApprox2 app1 app2 eps1 eps2` lifts `app1` and `app2` into the lifted type with the associated margins of error `eps1` and `eps2`.