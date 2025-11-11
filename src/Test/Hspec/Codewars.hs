-- |
-- Module      : Test.Hspec.Codewars
-- Description : Utility functions for testing on Codewars with Hspec
-- License     : MIT

{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Codewars (
  Hidden(..),
  solutionShouldHide,
  solutionShouldHideAll,
  shouldBeApproxPrec,
  shouldBeApprox
) where

import Data.List (intercalate)
import Text.Printf
import Test.Hspec
import Test.HUnit (assertBool)
import qualified Language.Haskell.Exts as Q
import Data.Approx (Approx(..))

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
shouldBeApproxPrec :: (Approx a, Show a) => a -> a -> a -> Expectation
shouldBeApproxPrec margin = shouldBeWithShow (isApprox margin) showFail
  where
    showFail actual expected = concat [
        "Test Failed"
      , "\nexpected: ", show expected, " within margin of ", show margin
      , "\n but got: ", show actual
      ]

infix 1 `shouldBeApprox`

-- | Predefined approximately equal expectation with error margin `1e-6`.
--
-- > sqrt 2.0 `shouldBeApprox` (1.4142135 :: Double)
shouldBeApprox :: (Fractional a, Approx a, Show a) => a -> a -> Expectation
shouldBeApprox = shouldBeApproxPrec 1e-6

-- | Like 'Test.Hspec.Expectations.shouldBe', but with an explicitly given
--   equality function.
--
--   The supplied equality function does not have to be symmetric, and shall be
--   called with the expected value as the second argument.
--
-- > shouldBeWith ((==) `on` sort) "abc" "cba"
shouldBeWith :: (Show a) => (a -> a -> Bool) -> a -> a -> Expectation
shouldBeWith eql = shouldBeWithShow eql showFail
  where
    showFail actual expected =
        "expected: " ++ show expected ++
      "\n but got: " ++ show actual

-- | Like 'shouldBeWith', but with an explicitly given show function for failed
--   tests.
shouldBeWithShow :: (a -> a -> Bool)
                 -> (a -> a -> String)
                 -> a -> a -> Expectation
shouldBeWithShow eql showFail actual expected =
  assertBool (showFail actual expected) (eql actual expected)
