module TransformSpec (main, spec) where

import Transform

import qualified GHC           as GHC
-- import qualified Outputable    as GHC

import Data.Algorithm.Diff
import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Types as GM
import Language.Haskell.Refact.Utils.Monad
import System.Directory


import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Test MRP" $ do
    it "mrp in Maybe1" $ do
     -- r <- ct $ mrp defaultTestSettings testOptions "./Maybe1.hs"
     r <- ct $ mrp logTestSettings testOptions "./Maybe1.hs"

     r' <- ct $ mapM makeRelativeToCurrentDirectory r

     r' `shouldBe` [ "Maybe1.hs"
                   ]

     diffM <- compareFiles "./testcases/H2010/Maybe1.expected.hs"
                           "./testcases/H2010/Maybe1.refactored.hs"
     diffM `shouldBe` []


-- ---------------------------------------------------------------------

ct :: IO a -> IO a
ct = cdAndDo "testcases/H2010"

cdAndDo :: FilePath -> IO a -> IO a
cdAndDo path fn = do
  old <- getCurrentDirectory
  r <- GHC.gbracket (setCurrentDirectory path) (\_ -> setCurrentDirectory old)
          $ \_ -> fn
  return r
-- ---------------------------------------------------------------------

testOptions :: GM.Options
testOptions = GM.defaultOptions {
    GM.optOutput     = GM.OutputOpts {
      GM.ooptLogLevel       = GM.GmError
      -- GM.ooptLogLevel       = GM.GmVomit
    , GM.ooptStyle          = GM.PlainStyle
    , GM.ooptLineSeparator  = GM.LineSeparator "\0"
    , GM.ooptLinePrefix     = Nothing
    }

    }

-- ---------------------------------------------------------------------

defaultTestSettings :: RefactSettings
defaultTestSettings = defaultSettings { rsetVerboseLevel = Normal }

logTestSettings :: RefactSettings
logTestSettings = defaultSettings { rsetVerboseLevel = Debug }

-- ---------------------------------------------------------------------

compareFiles :: FilePath -> FilePath -> IO [Diff [String]]
compareFiles fileA fileB = do
  astr <- readFile fileA
  bstr <- readFile fileB
  return $ compareStrings astr bstr

compareStrings :: String -> String -> [Diff [String]]
compareStrings astr bstr = filter (\c -> not( isBoth c)) $ getGroupedDiff (lines astr) (lines bstr)
    where
      isBoth (Both _ _) = True
      isBoth _        = False

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------
