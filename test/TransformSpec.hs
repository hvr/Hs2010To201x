module TransformSpec (main, spec) where

import           Test.Hspec

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "first feature" $ do
    it "first thing it does" $ do
      pendingWith "this is just a placeholder"
