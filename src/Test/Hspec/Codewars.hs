-- |
-- Module      : Test.Hspec.Codewars
-- Description : Utility functions for testing on Codewars with Hspec
-- License     : MIT

{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Codewars (
  Hidden(..),
  solutionShouldHide,
  solutionShouldHideAll,
  shouldBeApproxPrec, shouldBeApproxPrec1, shouldBeApproxPrec2, shouldBeApproxPrec2',
  shouldBeApprox, shouldBeApprox1, shouldBeApprox2, shouldBeApprox2',
  isApprox,
  Approx(..), Approx1(..), Approx2(..),
) where

import Data.Functor.Classes (Eq1 (..), Eq2 (liftEq2))
import Data.List (intercalate)
import Text.Printf
import Test.Hspec
import Test.HUnit (assertBool)
import qualified Language.Haskell.Exts as Q

ripParseOk :: Q.ParseResult a -> IO a
ripParseOk (Q.ParseOk x) = return x
ripParseOk _ = fail "Could not parse solution correctly"

getImports :: Q.Module a -> IO [Q.ImportDecl a]
getImports (Q.Module _ _ _ x _) = return x
getImports _ = fail "Unknown source type"

getModuleName :: Q.ModuleName a -> String
getModuleName (Q.ModuleName _ x) = x

nameToStr :: Q.Name a -> String
nameToStr (Q.Ident _ x) = x
nameToStr (Q.Symbol _ x) = x

cnameToStr :: Q.CName a -> String
cnameToStr (Q.VarName _ x) = nameToStr x
cnameToStr (Q.ConName _ x) = nameToStr x

specToStr :: Q.ImportSpec a -> [String]
specToStr (Q.IVar _ x) = [nameToStr x]
specToStr (Q.IAbs _ _ x) = [nameToStr x]
specToStr (Q.IThingAll _ x) = [nameToStr x]
specToStr (Q.IThingWith _ x cn) = nameToStr x : map cnameToStr cn

data ImportDesc =
  ImportAll {mName :: String}
  | ImportSome {mName :: String, mSymbols :: [String]}
  | HideSome {mName :: String, mSymbols :: [String]}
  deriving (Eq, Show)

declToDesc :: Q.ImportDecl a -> ImportDesc
declToDesc decl = case Q.importSpecs decl of
  Nothing -> ImportAll moduleName
  Just (Q.ImportSpecList _ True xs) -> HideSome moduleName (concatMap specToStr xs)
  Just (Q.ImportSpecList _ False xs) -> ImportSome moduleName (concatMap specToStr xs)
  where
    moduleName = getModuleName $ Q.importModule decl

treatPrelude :: [ImportDesc] -> [ImportDesc]
treatPrelude xs = if any (\x -> mName x == "Prelude") xs then xs else ImportAll "Prelude" : xs

data Hidden
  -- | Module to be hidden
  = Module {moduleName :: String}
  -- | Symbol from a module to be hidden
  | FromModule {moduleName :: String, symbolName :: String}
  deriving (Eq)

instance Show Hidden where
  show (Module{..}) = moduleName
  show (FromModule{..}) = moduleName ++ "." ++ symbolName
  showList hiddens xs = intercalate ", " (map show hiddens) ++ xs

exposed :: ImportDesc -> Hidden -> Bool
exposed (ImportAll{..}) (Module{..}) = mName == moduleName
exposed (ImportAll{..}) (FromModule{..}) = mName == moduleName
exposed (ImportSome{..}) (Module{..}) = mName == moduleName
exposed (ImportSome{..}) (FromModule{..}) = mName == moduleName && symbolName `elem` mSymbols
exposed (HideSome{..}) (Module{..}) = mName == moduleName
exposed (HideSome{..}) (FromModule{..}) = mName == moduleName && symbolName `notElem` mSymbols

hidden :: [Hidden] -> Expectation
hidden hiddens = do
  sol <- Q.parseFile "solution.txt" >>= ripParseOk >>= getImports
  let imports = treatPrelude $ map declToDesc sol
  let failures = [(desc, hide) | desc <- imports, hide <- hiddens, exposed desc hide]
  let message = "Import declarations must hide " ++ show hiddens
  assertBool message $ null failures

-- | Check that solution hides a module or a symbol from a module.
--
-- > solutionShouldHide $ FromModule "Prelude" "head"
solutionShouldHide :: Hidden -> Expectation
solutionShouldHide = hidden . pure

-- | Check that solution hides all of given modules and symbols.
--
-- > solutionShouldHideAll [FromModule "Prelude" "head", Module "Data.Set"]
solutionShouldHideAll :: [Hidden] -> Expectation
solutionShouldHideAll = hidden

-- | Create approximately equal expectation with margin.
--
-- > shouldBeApprox' = shouldBeApproxPrec 1e-9
shouldBeApproxPrec :: (Num a, Ord a, Show a) => a -> a -> a -> Expectation
shouldBeApproxPrec margin actual expected =
  Actual actual `shouldBe` Expected margin expected

infix 1 `shouldBeApprox`

-- | Predefined approximately equal expectation.
-- @actual \`shouldBeApprox\` expected@ sets the expectation that @actual@ is
-- approximately equal to @expected@ within the margin of @1e-6@.
--
-- > sqrt 2.0 `shouldBeApprox` (1.4142135 :: Double)
shouldBeApprox :: (Fractional a, Ord a, Show a) => a -> a -> Expectation
shouldBeApprox = shouldBeApproxPrec 1e-6

-- | Wrapper for values to be compared for approximate equality. The
-- 'Show' instance is altered so that failure messages also show the margin of
-- error.
data Approx a
  = Actual a
  | Expected a a -- ^ @Expected margin value@

-- | @isApprox margin actual expected@ determines if @actual@ is approximately
-- equal to @expected@ within the given @margin@.
isApprox :: (Num a, Ord a) => a -> a -> a -> Bool
isApprox margin actual expected =
  abs (actual - expected) <= abs margin * max 1 (abs expected)

instance (Num a, Ord a) => Eq (Approx a) where
  Actual a == Expected m e = isApprox m a e
  Expected m e == Actual a = isApprox m a e
  _ == _ = eqApproxError

eqApproxError :: a
eqApproxError = error "equality only supported between actual/expected values"

instance Show a => Show (Approx a) where
  showsPrec p (Actual a)     = showsPrec p a
  showsPrec p (Expected m e) =
    showParen (p > 10) $ shows e . showString " within margin of " . shows m

-- | Wrapper for values to be compared for approximate equality within a given
-- margin of error, lifted via the 'Eq1' instance of the unary type constructor.
data Approx1 t a
  = Actual1 (t a)
  | Expected1 a (t a)

instance (Num a, Ord a, Eq1 t) => Eq (Approx1 t a) where
  Actual1 a == Expected1 m e = liftEq (isApprox m) a e
  Expected1 m e == Actual1 a = liftEq (isApprox m) a e
  _ == _ = eqApproxError

instance (Show a, Show (t a)) => Show (Approx1 t a) where
  showsPrec p (Actual1 a) = showsPrec p a
  showsPrec p (Expected1 m e) =
    showParen (p > 10) $ shows e . showString " within margin of " . shows m

shouldBeApprox1 :: (Fractional a, Ord a, Show a, Show (t a), Eq1 t) => t a -> t a -> Expectation
shouldBeApprox1 = shouldBeApproxPrec1 1e-6

shouldBeApproxPrec1 :: (Num a, Ord a, Show a, Show (t a), Eq1 t) => a -> t a -> t a -> Expectation
shouldBeApproxPrec1 margin actual expected =
  Actual1 actual `shouldBe` Expected1 margin expected

-- | Wrapper for values to be compared for approximate equality within two given
-- margins of error, lifted via the 'Eq2' instance of the binary type
-- constructor.
data Approx2 t a b
  = Actual2 (t a b)
  | Expected2 a b (t a b)

instance (Num a, Ord a, Num b, Ord b, Eq2 t) => Eq (Approx2 t a b) where
  Actual2 a == Expected2 m1 m2 e = liftEq2 (isApprox m1) (isApprox m2) a e
  Expected2 m1 m2 e == Actual2 a = liftEq2 (isApprox m1) (isApprox m2) a e
  _ == _ = eqApproxError

instance (Show a, Show b, Show (t a b)) => Show (Approx2 t a b) where
  showsPrec p (Actual2 a) = showsPrec p a
  showsPrec p (Expected2 m1 m2 e) =
    showParen (p > 10) $
      shows e .
      showString " within margins of " . shows m1 .
      showString " and " . shows m2

shouldBeApproxPrec2 :: (Num a, Ord a, Num b, Ord b, Show a, Show b, Show (t a b), Eq2 t) => a -> b -> t a b -> t a b -> Expectation
shouldBeApproxPrec2 margin1 margin2 actual expected =
  Actual2 actual `shouldBe` Expected2 margin1 margin2 expected

shouldBeApprox2 :: (Fractional a, Ord a, Fractional b, Ord b, Show a, Show b, Show (t a b), Eq2 t) => t a b -> t a b -> Expectation
shouldBeApprox2 = shouldBeApproxPrec2 1e-6 1e-6

shouldBeApproxPrec2' :: (Show a, Show (t a a), Num a, Ord a, Eq2 t) => a -> t a a -> t a a -> Expectation
shouldBeApproxPrec2' margin actual expected =
  Actual2 actual `shouldBe` Expected2 margin margin expected

shouldBeApprox2' :: (Fractional a, Show a, Show (t a a), Ord a, Eq2 t) => t a a -> t a a -> Expectation
shouldBeApprox2' = shouldBeApproxPrec2' 1e-6