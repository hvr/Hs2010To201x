module MRPSpec (main, spec) where

import Language.Haskell.Migrate.MRP

import qualified GHC           as GHC
-- import qualified Outputable    as GHC

import Data.Algorithm.Diff
import System.Directory
import Control.Logging


import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Test MRP" $ do
    it "mrp in Maybe1" $ do
     ct $ mrp testOpts "./Maybe1.hs"

     diffM <- compareFiles "./testcases/H2010/Maybe1.expected.hs"
                           "./testcases/H2010/Maybe1.refactored.hs"
     diffM `shouldBe` []

    -- ---------------------------------

    it "mrp in Maybe3 multi-param" $ do
     ct $ mrp testOpts "./Maybe3.hs"

     diffM <- compareFiles "./testcases/H2010/Maybe3.expected.hs"
                           "./testcases/H2010/Maybe3.refactored.hs"
     diffM `shouldBe` []

-- ---------------------------------------------------------------------

testOpts :: Opts
testOpts = Opts [] (PreSuffix "refactored") False

debugTestOpts :: Opts
debugTestOpts = Opts [] (PreSuffix "refactored") True

-- ---------------------------------------------------------------------

ct :: IO a -> IO a
ct = cdAndDo "testcases/H2010"

cdAndDo :: FilePath -> IO a -> IO a
cdAndDo path fn = do
  old <- getCurrentDirectory
  r <- GHC.gbracket (setCurrentDirectory path) (\_ -> setCurrentDirectory old)
          $ \_ -> withStdoutLogging (setLogLevel LevelError >> fn)
  return r

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
