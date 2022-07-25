module Main (main) where

import Data.HList
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "show" $ do
    it "shows an empty list" $
      show HNil `shouldBe` "[]"

    it "shows a list" $
      show (1 `HCons` 'a' `HCons` HNil) `shouldBe` "[1, 'a']"

  describe "length" $ do
    it "length" $
      hlength (42 `HCons` 'h' `HCons` HNil)
        `shouldBe` 2

  describe "append" $ do
    it "appends empty lists" $
      happend HNil HNil `shouldBe` HNil

    it "appends list to empty list " $
      let xs = HNil
          ys = 1 `HCons` 'b' `HCons` HNil
       in xs `happend` ys `shouldBe` ys

    it "appends lists" $
      let xs = 'a' `HCons` True `HCons` HNil
          ys = 1 `HCons` 'b' `HCons` HNil
          zs = 'a' `HCons` True `HCons` 1 `HCons` 'b' `HCons` HNil
       in xs `happend` ys `shouldBe` zs
