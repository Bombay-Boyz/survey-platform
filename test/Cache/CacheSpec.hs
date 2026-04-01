module Cache.CacheSpec (spec) where

import Control.Concurrent  (threadDelay)

import Test.Hspec

import Cache

spec :: Spec
spec = describe "Cache" $ do

  describe "getCached / setCached" $ do
    it "returns Nothing for a missing key" $ do
      cache <- newCache
      result <- getCached (cache :: Cache Int) "missing"
      result `shouldBe` Nothing
    it "returns the value immediately after setting" $ do
      cache <- newCache
      setCached cache "k" (42 :: Int) 60
      result <- getCached cache "k"
      result `shouldBe` Just 42
    it "overwrites an existing key" $ do
      cache <- newCache
      setCached cache "k" (1 :: Int) 60
      setCached cache "k" (2 :: Int) 60
      result <- getCached cache "k"
      result `shouldBe` Just 2
    it "returns Nothing for an expired entry" $ do
      cache <- newCache
      setCached cache "k" (99 :: Int) 0.001
      threadDelay 5000
      result <- getCached cache "k"
      result `shouldBe` Nothing

  describe "invalidate" $ do
    it "removes an existing key" $ do
      cache <- newCache
      setCached cache "k" (7 :: Int) 60
      invalidate cache "k"
      result <- getCached cache "k"
      result `shouldBe` Nothing
    it "is a no-op for a missing key" $ do
      cache <- newCache
      invalidate (cache :: Cache Int) "nonexistent"
      result <- getCached cache "nonexistent"
      result `shouldBe` Nothing

  describe "purgeExpired" $ do
    it "removes expired entries without affecting live ones" $ do
      cache <- newCache
      setCached cache "live"    (1 :: Int) 60
      setCached cache "expired" (2 :: Int) 0.001
      threadDelay 5000
      purgeExpired cache
      live    <- getCached cache "live"
      expired <- getCached cache "expired"
      live    `shouldBe` Just 1
      expired `shouldBe` Nothing
