{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Clash.Prelude hiding (resize)
import Control.Monad.State (runState)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import PoolAllocator
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen, Property, (===), chooseInteger, conjoin, counterexample
    , forAll, listOf, oneof, resize )

type Payload = BitVector 8
type Payload16 = BitVector 16

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

activeFree :: KnownNat (n + 1) => AllocatorState n -> [Index n]
activeFree st =
    L.take (fromIntegral (freeCount st)) (toList (freeBlockList st))

allBlocks :: KnownNat n => [Index n]
allBlocks = toList indicesI

genCmd3 :: Gen (AllocatorCmd 3 Payload)
genCmd3 = oneof
    [ pure Nop
    , Alloc . fromIntegral <$> chooseInteger (0, 255)
    , Free . fromIntegral <$> chooseInteger (0, 2)
    , ReadBlockData . fromIntegral <$> chooseInteger (0, 2)
    ]

genCmds3 :: Gen [AllocatorCmd 3 Payload]
genCmds3 = resize 30 (listOf genCmd3)

-- Blocks that are allocated after each command, derived from the responses:
-- a successful 'Alloc' adds a block, an accepted ('Waiting') 'Free' removes it
liveBlocks :: [AllocatorCmd 3 Payload] -> [[Index 3]]
liveBlocks = go (initAllocatorState @3) []
  where
    go _ _ [] = []
    go st live (c : cs) =
        let (answer, st') = runStep st c
            live' = case (c, answer) of
                (Alloc _, (Just (i, _), _, AllocSuccess _)) -> i : live
                (Free i, (Nothing, Nothing, Waiting))       -> L.delete i live
                _                                           -> live
        in live' : go st' live' cs

-- The free list never contains an index twice
prop_freeListUnique :: [AllocatorCmd 3 Payload] -> Property
prop_freeListUnique cmds =
    conjoin
        [ counterexample
            (unwords ["duplicate free-list entry after step", show k])
            (L.nub (activeFree st) == activeFree st)
        | (k, st) <- L.zip [0 :: Int ..] (states (runSteps cmds))
        ]

-- No block is ever handed out to a second owner while it is still live
prop_noAliasing :: [AllocatorCmd 3 Payload] -> Property
prop_noAliasing cmds =
    conjoin
        [ counterexample
            (unwords
                ["block handed out twice by step", show k, ":", show live])
            (L.nub live == live)
        | (k, live) <- L.zip [0 :: Int ..] (liveBlocks cmds)
        ]

-- At every step the free list is exactly the complement of the live blocks
prop_freeListMatchesLive :: [AllocatorCmd 3 Payload] -> Property
prop_freeListMatchesLive cmds =
    conjoin
        [ counterexample
            (unwords
                [ "free list does not complement the live blocks after step"
                , show k, ":", show st, show live ])
            (L.sort (activeFree st) == L.sort (allBlocks @3 L.\\ live))
        | (k, (st, live)) <-
            L.zip [0 :: Int ..]
                  (L.zip (states (runSteps cmds)) (liveBlocks cmds))
        ]

-- A rejected free leaves the allocator state untouched
prop_rejectedFreeIsNoOp :: [AllocatorCmd 3 Payload] -> Property
prop_rejectedFreeIsNoOp cmds =
    let steps = runSteps cmds
        statesBefore = initAllocatorState @3 : L.map snd (L.init steps)
    in conjoin
        [ counterexample
            (unwords
                ["rejected free changed the state after step", show k])
            (stBefore == stAfter)
        | (k, (answer, stAfter), stBefore) <-
            L.zip3 [0 :: Int ..] steps statesBefore
        , isInvalidFree answer
        ]
  where
    isInvalidFree (_, _, InvalidFree) = True
    isInvalidFree _                   = False

prop_hardwareMatchesModel :: [AllocatorCmd 3 Payload] -> Property
prop_hardwareMatchesModel cmds =
    let (ans, _) = runPool cmds
    in ans === L.map (\(_, _, r) -> r) (answers (runSteps cmds))

nopCmd :: AllocatorCmd n Payload
nopCmd = Nop

allocCmd :: Payload -> AllocatorCmd n Payload
allocCmd = Alloc

freeCmd :: Index n -> AllocatorCmd n Payload
freeCmd = Free

readCmd :: Index n -> AllocatorCmd n Payload
readCmd = ReadBlockData

atIndex :: Int -> [a] -> Maybe a
atIndex i = listToMaybe . L.drop i

runPool
    :: (KnownNat n, NFDataX a)
    => [AllocatorCmd n a]
    -> ([AllocatorRes n a], [a])
runPool cmds =
    withClockResetEnable systemClockGen systemResetGen enableGen $
        let (res, dat) = poolAllocator (fromList (Nop : cmds))
            cycles = L.length cmds + 1
        in (L.tail (sampleN cycles res), L.tail (sampleN cycles dat))

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
                (_, st2) = runStep st1 (allocCmd 2)
                (_, st3) = runStep st2 (allocCmd 3)
                (answer, st4) = runStep st3 (freeCmd 3)
            answer `shouldBe` (Nothing, Nothing, Waiting)

            st4 `shouldBe` AllocatorState (0 :> 3 :> 2 :> 3 :> Nil) 2

            fst (runStep st4 (allocCmd 4))
                `shouldBe` (Just (3, 4), Nothing, AllocSuccess 3)

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

    describe "allocatorStep/InvalidFree" $ do
        it "rejects a free while the whole pool is free" $ do
            let steps = runSteps ([freeCmd 3, freeCmd 0] :: [AllocatorCmd 4 Payload])
            answers steps
                `shouldBe` [ (Nothing, Nothing, InvalidFree)
                           , (Nothing, Nothing, InvalidFree)
                           ]
            states steps
                `shouldBe` [ AllocatorState indicesI 4
                           , AllocatorState indicesI 4
                           ]

        it "rejects a double free and still hands out distinct blocks" $ do
            let steps = runSteps
                    ([ allocCmd 42, freeCmd 1, freeCmd 1
                     , allocCmd 7, allocCmd 9
                     ] :: [AllocatorCmd 2 Payload])
            answers steps
                `shouldBe` [ (Just (1, 42), Nothing, AllocSuccess 1)
                           , (Nothing, Nothing, Waiting)
                           , (Nothing, Nothing, InvalidFree)
                           , (Just (1, 7), Nothing, AllocSuccess 1)
                           , (Just (0, 9), Nothing, AllocSuccess 0)
                           ]

        it "keeps the free list free of duplicates after repeated frees" $ do
            let steps = runSteps
                    ([ allocCmd 1, allocCmd 2, freeCmd 1, freeCmd 1, freeCmd 1]
                     :: [AllocatorCmd 2 Payload])
                st = snd (L.last steps)
            activeFree st `shouldBe` [1]
            L.nub (activeFree st) `shouldBe` activeFree st

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
                    ([ allocCmd 10, allocCmd 20, allocCmd 30, nopCmd, readCmd 1
                     , freeCmd 2, freeCmd 3, allocCmd 40, allocCmd 50, allocCmd 60
                     ] :: [AllocatorCmd 4 Payload])

            answers steps
                `shouldBe` [ (Just (3, 10), Nothing, AllocSuccess 3)
                            , (Just (2, 20), Nothing, AllocSuccess 2)
                            , (Just (1, 30), Nothing, AllocSuccess 1)
                            , (Nothing, Nothing, Waiting)
                            , (Nothing, Just 1, Waiting)
                            , (Nothing, Nothing, Waiting)
                            , (Nothing, Nothing, Waiting)
                            , (Just (3, 40), Nothing, AllocSuccess 3)
                            , (Just (2, 50), Nothing, AllocSuccess 2)
                            , (Just (0, 60), Nothing, AllocSuccess 0)
                            ]
            states steps
                `shouldBe` [ AllocatorState indicesI 3
                            , AllocatorState indicesI 2
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 1
                            , AllocatorState indicesI 1
                            , AllocatorState (0 :> 2 :> 2 :> 3 :> Nil) 2
                            , AllocatorState (0 :> 2 :> 3 :> 3 :> Nil) 3
                            , AllocatorState (0 :> 2 :> 3 :> 3 :> Nil) 2
                            , AllocatorState (0 :> 2 :> 3 :> 3 :> Nil) 1
                            , AllocatorState (0 :> 2 :> 3 :> 3 :> Nil) 0
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

    describe "allocatorStep/invariants (QuickCheck)" $ do
        prop "never keeps duplicate entries in the free list"
            (forAll genCmds3 prop_freeListUnique)

        prop "never hands the same block to two owners"
            (forAll genCmds3 prop_noAliasing)

        prop "keeps the free list the exact complement of the live blocks"
            (forAll genCmds3 prop_freeListMatchesLive)

        prop "leaves the state unchanged when a free is rejected"
            (forAll genCmds3 prop_rejectedFreeIsNoOp)

    describe "poolAllocator (hardware simulation)" $ do
        it "answers Waiting on Nop" $ do
            let (ans, _) = runPool ([nopCmd, nopCmd] :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [Waiting, Waiting]

        it "allocates blocks in LIFO order and then reports NoFreeBlocks" $ do
            let (ans, _) = runPool
                    ([allocCmd 1, allocCmd 2, allocCmd 3] :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [AllocSuccess 1, AllocSuccess 0, NoFreeBlocks]

        it "hands out a freed block again" $ do
            let (ans, _) = runPool
                    ([allocCmd 1, allocCmd 2, freeCmd 0, allocCmd 3]
                     :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [AllocSuccess 1, AllocSuccess 0, Waiting, AllocSuccess 0]

        it "stores payloads in the block RAM and reads them back one cycle later" $ do
            let (ans, dat) = runPool
                    ([allocCmd 20, allocCmd 10, readCmd 0, readCmd 1, nopCmd]
                     :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [AllocSuccess 1, AllocSuccess 0, Waiting, Waiting, Waiting]
            L.drop 3 dat `shouldBe` [10, 20]

        it "agrees with the allocatorStep model on a command sequence" $ do
            let cmds = [ allocCmd 10, nopCmd, readCmd 2, freeCmd 2
                       , allocCmd 20, freeCmd 1, allocCmd 30, allocCmd 31
                       ] :: [AllocatorCmd 4 Payload]
                (ans, _) = runPool cmds
            ans `shouldBe` L.map (\(_, _, r) -> r) (answers (runSteps cmds))

        it "agrees with the allocatorStep model on a longer mixed sequence" $ do
            let cmds = [ allocCmd 10, nopCmd, readCmd 2, freeCmd 2, allocCmd 20
                       , freeCmd 1, allocCmd 30, allocCmd 31, readCmd 0
                       , freeCmd 3, allocCmd 40
                       ] :: [AllocatorCmd 4 Payload]
                (ans, _) = runPool cmds
            ans `shouldBe` L.map (\(_, _, r) -> r) (answers (runSteps cmds))

        it "agrees with the allocatorStep model when the pool is exhausted and reused" $ do
            let cmds = [ allocCmd 1, allocCmd 2, allocCmd 3
                       , freeCmd 0, freeCmd 1, allocCmd 4, allocCmd 5, allocCmd 6
                       ] :: [AllocatorCmd 2 Payload]
                (ans, _) = runPool cmds
            ans `shouldBe` L.map (\(_, _, r) -> r) (answers (runSteps cmds))

        it "returns the payload stored in each block (RAM address decode)" $ do
            let (ans, dat) = runPool
                    ([ allocCmd 11, allocCmd 22, allocCmd 33, allocCmd 44
                     , nopCmd, readCmd 0, readCmd 1, readCmd 2, readCmd 3, nopCmd
                     ] :: [AllocatorCmd 4 Payload])
            ans `shouldBe` [ AllocSuccess 3, AllocSuccess 2
                           , AllocSuccess 1, AllocSuccess 0
                           , Waiting, Waiting, Waiting, Waiting, Waiting, Waiting
                           ]
            L.drop 6 dat `shouldBe` [44, 33, 22, 11]

        it "keeps a freed block's payload until it is reallocated" $ do
            let (ans, dat) = runPool
                    ([ allocCmd 42, allocCmd 7, freeCmd 1, readCmd 1
                     , allocCmd 9, nopCmd, readCmd 1, nopCmd
                     ] :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [ AllocSuccess 1, AllocSuccess 0, Waiting, Waiting
                           , AllocSuccess 1, Waiting, Waiting, Waiting
                           ]
            atIndex 4 dat `shouldBe` Just 42
            atIndex 7 dat `shouldBe` Just 9

        it "works for a one block pool" $ do
            let (ans, dat) = runPool
                    ([allocCmd 7, readCmd 0, readCmd 0, allocCmd 9]
                     :: [AllocatorCmd 1 Payload])
            ans `shouldBe` [AllocSuccess 0, Waiting, Waiting, NoFreeBlocks]
            L.drop 2 dat `shouldBe` [7, 7]

        it "supports a payload wider than the 8-bit test payload" $ do
            let (ans, dat) = runPool
                    ([ Alloc 0xBEEF, Alloc 0xCAFE, Nop
                     , ReadBlockData 0, ReadBlockData 1, Nop
                     ] :: [AllocatorCmd 2 Payload16])
            ans `shouldBe` [ AllocSuccess 1, AllocSuccess 0
                           , Waiting, Waiting, Waiting, Waiting
                           ]
            L.drop 4 dat `shouldBe` [0xCAFE, 0xBEEF]

        it "reports InvalidFree instead of crashing on a free of a free block" $ do
            let (ans, _) = runPool
                    ([freeCmd 0, freeCmd 1, allocCmd 1, allocCmd 2, allocCmd 3]
                     :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [ InvalidFree, InvalidFree
                           , AllocSuccess 1, AllocSuccess 0, NoFreeBlocks
                           ]

        it "does not hand out the same block twice after a double free" $ do
            let (ans, _) = runPool
                    ([allocCmd 42, freeCmd 1, freeCmd 1, allocCmd 7, allocCmd 9]
                     :: [AllocatorCmd 2 Payload])
            ans `shouldBe` [ AllocSuccess 1, Waiting, InvalidFree
                           , AllocSuccess 1, AllocSuccess 0
                           ]

        it "keeps the RAM contents across frees and reallocations" $ do
            let (ans, dat) = runPool
                    ([ allocCmd 7, allocCmd 8, allocCmd 9, freeCmd 1
                     , allocCmd 10, nopCmd, readCmd 2, readCmd 1, readCmd 0, nopCmd
                     ] :: [AllocatorCmd 3 Payload])
            ans `shouldBe` [ AllocSuccess 2, AllocSuccess 1, AllocSuccess 0
                           , Waiting, AllocSuccess 1, Waiting
                           , Waiting, Waiting, Waiting, Waiting
                           ]
            L.drop 7 dat `shouldBe` [7, 10, 9]

        prop "matches the allocatorStep model over generated sequences"
            (forAll genCmds3 prop_hardwareMatchesModel)
