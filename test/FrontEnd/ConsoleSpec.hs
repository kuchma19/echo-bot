{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.ConsoleSpec
  ( spec,
  )
where

import Test.Hspec (Spec, shouldBe, it)

spec :: Spec
spec = it "console test" $ do
    True `shouldBe` True