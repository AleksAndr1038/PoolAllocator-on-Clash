{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clash.Prelude
import Control.Monad.State (runState)
import qualified Data.List as L
import PoolAllocator
import Test.Hspec

type Payload = BitVector 8

type Step n a =
    ( Maybe (Index n, a)
    , Maybe (Index n)
    , AllocatorRes n a
    )

runStep
    :: (KnownNat n, NFDataX a)
    => AllocatorState n
    -> AllocatorCmd n a
    -> (Step n a, AllocatorState n)
runStep st cmd = runState (allocatorStep cmd) st

stepState
    :: (KnownNat n, NFDataX a)
    => AllocatorState n
    -> AllocatorCmd n a
    -> AllocatorState n
stepState st = snd . runStep st

runSteps
    :: (KnownNat n, NFDataX a)
    => [AllocatorCmd n a]
    -> [(Step n a, AllocatorState n)]
runSteps = go initAllocatorState
  where
    go _ []        = []
    go st (c : cs) = case runStep st c of
        (answer, st') -> (answer, st') : go st' cs

answers :: [(Step n a, AllocatorState n)] -> [Step n a]
answers = L.map fst

states :: [(Step n a, AllocatorState n)] -> [AllocatorState n]
states = L.map snd

nopCmd :: AllocatorCmd n Payload
nopCmd = Nop

allocCmd :: Integer -> AllocatorCmd n Payload
allocCmd d = Alloc (fromIntegral d)

freeCmd :: KnownNat n => Integer -> AllocatorCmd n Payload
freeCmd i = Free (fromIntegral i)

readCmd :: KnownNat n => Integer -> AllocatorCmd n Payload
readCmd i = ReadBlockData (fromIntegral i)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "initAllocatorState (state allocatorStep starts from)" $ do
        it "marks every block of the pool as free" $ do
            initAllocatorState @4 `shouldBe` AllocatorState indicesI 4

        it "works for a one block pool" $ do
            initAllocatorState @1 `shouldBe` AllocatorState indicesI 1

    describe "allocatorStep/Nop" $ do
        it "answers Waiting without a write or a read" $ do
            let (answer, _) = runStep (initAllocatorState @4 :: AllocatorState 4) nopCmd
            answer `shouldBe` (Nothing, Nothing, Waiting)

        it "keeps the state untouched" $ do
            let (_, st') = runStep (initAllocatorState @4 :: AllocatorState 4) nopCmd
            st' `shouldBe` AllocatorState indicesI 4

        it "keeps an exhausted state exhausted" $ do
            let st0 = initAllocatorState @1 :: AllocatorState 1
                (_, st1) = runStep st0 (allocCmd 1)
                (answer, st2) = runStep st1 nopCmd
            answer `shouldBe` (Nothing, Nothing, Waiting)
            st2 `shouldBe` AllocatorState indicesI 0

    describe "allocatorStep/Alloc" $ do
        it "allocates the block on top of the free list" $ do
            let (answer, st') = runStep (initAllocatorState @4 :: AllocatorState 4) (allocCmd 42)
            answer `shouldBe` (Just (3, 42), Nothing, AllocSuccess 3)
            st' `shouldBe` AllocatorState indicesI 3

        it "returns the user data next to the block index" $ do
            let (answer, _) = runStep (initAllocatorState @2 :: AllocatorState 2) (allocCmd 171)
            answer `shouldBe` (Just (1, 171), Nothing, AllocSuccess 1)

        it "allocates a one block pool" $ do
            let (answer, st') = runStep (initAllocatorState @1 :: AllocatorState 1) (allocCmd 7)
            answer `shouldBe` (Just (0, 7), Nothing, AllocSuccess 0)
            st' `shouldBe` AllocatorState indicesI 0

        it "does not change the free block list, only the free block count" $ do
            stepState (initAllocatorState @3 :: AllocatorState 3) (allocCmd 9)
                `shouldBe` AllocatorState indicesI 2

        it "allocates blocks in LIFO order and then reports NoFreeBlocks" $ do
            let steps = runSteps
                    ([ allocCmd 1, allocCmd 2, allocCmd 3, allocCmd 4, allocCmd 5
                     ] :: [AllocatorCmd 3 Payload])
            answers steps
                `shouldBe` [ (Just (2, 1), Nothing, AllocSuccess 2)
                            , (Just (1, 2), Nothing, AllocSuccess 1)
                            , (Just (0, 3), Nothing, AllocSuccess 0)
                            , (Nothing, Nothing, NoFreeBlocks)
                            , (Nothing, Nothing, NoFreeBlocks)
                            ]
            states steps
                `shouldBe` [ AllocatorState indicesI 2
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 0
                            , AllocatorState indicesI 0
                            , AllocatorState indicesI 0
                            ]

    describe "allocatorStep/Free" $ do
        it "answers Waiting without a write or a read" $ do
            let st0 = initAllocatorState @4 :: AllocatorState 4
                (_, st1) = runStep st0 (allocCmd 1)
                (answer, _) = runStep st1 (freeCmd 3)
            answer `shouldBe` (Nothing, Nothing, Waiting)

        it "stores the freed block on top of the free list" $ do
            let st0 = initAllocatorState @4 :: AllocatorState 4
                (_, st1) = runStep st0 (allocCmd 1)
                (_, st2) = runStep st1 (freeCmd 1)
            st2 `shouldBe` AllocatorState (0 :> 1 :> 2 :> 1 :> Nil) 4

        it "restores the initial free list when the last block is freed" $ do
            let st0 = initAllocatorState @3 :: AllocatorState 3
                (_, st1) = runStep st0 (allocCmd 1)
                (_, st2) = runStep st1 (freeCmd 2)
            st2 `shouldBe` st0

        it "grows the free block count again" $ do
            let st0 = initAllocatorState @3 :: AllocatorState 3
                (_, st1) = runStep st0 (allocCmd 1)
                (_, st2) = runStep st1 (allocCmd 2)
                (_, st3) = runStep st2 (freeCmd 1)
            st3 `shouldBe` AllocatorState indicesI 2

        it "lets the allocator hand out the freed block again" $ do
            let st0 = initAllocatorState @2 :: AllocatorState 2
                (_, st1) = runStep st0 (allocCmd 1)
                (_, st2) = runStep st1 (allocCmd 2)
                (_, st3) = runStep st2 (freeCmd 0)
                (answer, st4) = runStep st3 (allocCmd 3)
            answer `shouldBe` (Just (0, 3), Nothing, AllocSuccess 0)
            st4 `shouldBe` AllocatorState indicesI 0

        it "frees several blocks in LIFO order" $ do
            let steps = runSteps
                    ([ allocCmd 1, allocCmd 2, allocCmd 3
                     , freeCmd 0, freeCmd 1, allocCmd 4, allocCmd 5
                     ] :: [AllocatorCmd 3 Payload])
            answers steps
                `shouldBe` [ (Just (2, 1), Nothing, AllocSuccess 2)
                            , (Just (1, 2), Nothing, AllocSuccess 1)
                            , (Just (0, 3), Nothing, AllocSuccess 0)
                            , (Nothing, Nothing, Waiting)
                            , (Nothing, Nothing, Waiting)
                            , (Just (1, 4), Nothing, AllocSuccess 1)
                            , (Just (0, 5), Nothing, AllocSuccess 0)
                            ]
            states steps
                `shouldBe` [ AllocatorState indicesI 2
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 0
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 2
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 0
                            ]

    describe "allocatorStep/ReadBlockData" $ do
        it "returns the requested index on the read port" $ do
            let (answer, _) = runStep (initAllocatorState @4 :: AllocatorState 4) (readCmd 2)
            answer `shouldBe` (Nothing, Just 2, Waiting)

        it "keeps the state untouched" $ do
            let (_, st') = runStep (initAllocatorState @4 :: AllocatorState 4) (readCmd 0)
            st' `shouldBe` AllocatorState indicesI 4

        it "does not release or reserve a block" $ do
            let st0 = initAllocatorState @2 :: AllocatorState 2
                (_, st1) = runStep st0 (allocCmd 1)
                (answer, st2) = runStep st1 (readCmd 1)
            answer `shouldBe` (Nothing, Just 1, Waiting)
            st2 `shouldBe` AllocatorState indicesI 1

    describe "allocatorStep/NoFreeBlocks" $ do
        it "answers NoFreeBlocks and keeps the exhausted state" $ do
            let st0 = initAllocatorState @2 :: AllocatorState 2
                (_, st1) = runStep st0 (allocCmd 1)
                (_, st2) = runStep st1 (allocCmd 2)
                (answer, st3) = runStep st2 (allocCmd 3)
            answer `shouldBe` (Nothing, Nothing, NoFreeBlocks)
            st3 `shouldBe` AllocatorState indicesI 0

        it "serves requests again after a block was freed" $ do
            let st0 = initAllocatorState @2 :: AllocatorState 2
                (_, st1) = runStep st0 (allocCmd 1)
                (_, st2) = runStep st1 (allocCmd 2)
                (_, st3) = runStep st2 (freeCmd 1)
                (answer, _) = runStep st3 (allocCmd 3)
            answer `shouldBe` (Just (1, 3), Nothing, AllocSuccess 1)

    describe "allocatorStep/command sequence" $ do
        it "keeps write port, read port and free list consistent" $ do
            let steps = runSteps
                    ([ allocCmd 10, nopCmd, readCmd 2, freeCmd 2
                     , allocCmd 20, freeCmd 1, allocCmd 30, allocCmd 31
                     ] :: [AllocatorCmd 4 Payload])
            answers steps
                `shouldBe` [ (Just (3, 10), Nothing, AllocSuccess 3)
                            , (Nothing, Nothing, Waiting)
                            , (Nothing, Just 2, Waiting)
                            , (Nothing, Nothing, Waiting)
                            , (Just (2, 20), Nothing, AllocSuccess 2)
                            , (Nothing, Nothing, Waiting)
                            , (Just (1, 30), Nothing, AllocSuccess 1)
                            , (Just (2, 31), Nothing, AllocSuccess 2)
                            ]
            states steps
                `shouldBe` [ AllocatorState indicesI 3
                            , AllocatorState indicesI 3
                            , AllocatorState indicesI 3
                            , AllocatorState (0 :> 1 :> 2 :> 2 :> Nil) 4
                            , AllocatorState (0 :> 1 :> 2 :> 2 :> Nil) 3
                            , AllocatorState (0 :> 1 :> 2 :> 1 :> Nil) 4
                            , AllocatorState (0 :> 1 :> 2 :> 1 :> Nil) 3
                            , AllocatorState (0 :> 1 :> 2 :> 1 :> Nil) 2
                            ]

        it "hands out both blocks again after a full free cycle" $ do
            let steps = runSteps
                    ([ allocCmd 1, allocCmd 2, freeCmd 0, freeCmd 1
                     , allocCmd 3, allocCmd 4
                     ] :: [AllocatorCmd 2 Payload])
            answers steps
                `shouldBe` [ (Just (1, 1), Nothing, AllocSuccess 1)
                            , (Just (0, 2), Nothing, AllocSuccess 0)
                            , (Nothing, Nothing, Waiting)
                            , (Nothing, Nothing, Waiting)
                            , (Just (1, 3), Nothing, AllocSuccess 1)
                            , (Just (0, 4), Nothing, AllocSuccess 0)
                            ]
            states steps
                `shouldBe` [ AllocatorState indicesI 1
                            , AllocatorState indicesI 0
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 2
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 0
                            ]

    describe "allocatorStep/idempotence of non allocating commands" $ do
        it "repeating Nop and ReadBlockData does not consume blocks" $ do
            let cmds = L.replicate 5 nopCmd
                    L.++ [readCmd 0, readCmd 0]
                    L.++ L.replicate 5 nopCmd :: [AllocatorCmd 4 Payload]
                steps = runSteps cmds
                nopAnswer = (Nothing, Nothing, Waiting) :: Step 4 Payload
                readAnswer = (Nothing, Just 0, Waiting) :: Step 4 Payload
            answers steps `shouldBe` L.replicate 5 nopAnswer
                L.++ [readAnswer, readAnswer]
                L.++ L.replicate 5 nopAnswer
            states steps `shouldBe` L.replicate 12 (AllocatorState indicesI 4)

        it "still allocates after a long run of non allocating commands" $ do
            let cmds = L.replicate 8 nopCmd
                    L.++ [allocCmd 1]
                    L.++ L.replicate 8 nopCmd :: [AllocatorCmd 2 Payload]
                steps = runSteps cmds
            answers steps L.!! 8 `shouldBe` (Just (1, 1), Nothing, AllocSuccess 1)
            states steps L.!! 8 `shouldBe` AllocatorState indicesI 1
            answers steps L.!! 9 `shouldBe` (Nothing, Nothing, Waiting)
            states steps L.!! 9 `shouldBe` AllocatorState indicesI 1
