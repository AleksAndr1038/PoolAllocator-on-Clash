{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import PoolAllocator
import Clash.Prelude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "PoolAllocator" $ do
        it "allocates a single block" $ do
            let alloc = createPool @5 8
                (_, result) = allocate alloc
            result `shouldBe` Just 0

        it "allocates multiple blocks in order" $ do
            let alloc = createPool @3 8
                (a1, i1) = allocate alloc
                (a2, i2) = allocate a1
                (_, i3) = allocate a2
            (i1, i2, i3) `shouldBe` (Just 0, Just 1, Just 2)

        it "returns Nothing when pool is exhausted" $ do
            let alloc = createPool @2 8
                (a1, _) = allocate alloc
                (a2, _) = allocate a1
                (_, result) = allocate a2
            result `shouldBe` Nothing
        
        it "reuses deallocated block" $ do
            let alloc = createPool @2 8
                (a1, i1) = case allocate alloc of
                    (nextAlloc, Just x) -> (nextAlloc, x)
                    (_, Nothing) -> error "Expected Just a1"

                (a2, _) = allocate a1
                a3 = deallocate a2 i1
                (_, res) = allocate a3

            (res, i1) `shouldBe` (Just 0, 0)

        it "handles deallocate and allocate sequence correctly" $ do
            let alloc = createPool @3 8
                (a1, i1) = case allocate alloc of
                    (nextAlloc, Just x) -> (nextAlloc, x)
                    (_, Nothing) -> error "Expected Just a1"

                (a2, i2) = case allocate a1 of
                    (nextAlloc, Just x) -> (nextAlloc, x)
                    (_, Nothing) -> error "Expected Just a2"

                a3 = deallocate a2 i1

                (_, i3) = case allocate a3 of
                    (nextAlloc, Just x) -> (nextAlloc, x)
                    (_, Nothing) -> error "Expected Just a4"

            (i1, i2, i3) `shouldBe` (0, 1, 0)

        it "free list grows after deallocation" $ do
            let alloc = createPool @1 8
                (a1, i1) = case allocate alloc of
                    (nextAlloc, Just x) -> (nextAlloc, x)
                    (_, Nothing) -> error "Expected Just a1"
                
                a2 = deallocate a1 i1

                (_, res) = allocate a2

            res `shouldBe` Just 0
