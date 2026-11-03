{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}


module PoolAllocator where

import Clash.Prelude
import Control.Monad.State
import Data.Maybe (fromMaybe)

data AllocatorCmd n a
    = Nop
    | Alloc a
    | Free (Index n)
    | ReadBlockData (Index n)
    deriving (Eq, Show, Generic, NFDataX)

data AllocatorRes n a
    = Waiting
    | AllocSuccess (Index n)
    | NoFreeBlocks
    | InvalidFree
    deriving (Eq, Show, Generic, NFDataX)

data AllocatorState n = AllocatorState {
    freeBlockList :: Vec n (Index n),
    freeCount :: Index (n + 1)
} deriving (Eq, Show, Generic, NFDataX)

initAllocatorState :: (KnownNat n) => AllocatorState n
initAllocatorState = AllocatorState {
    freeBlockList = indicesI,
    freeCount = maxBound
}

allocatorStep
    :: forall n a.
       (KnownNat n, NFDataX a)
    => AllocatorCmd n a
    -> State (AllocatorState n)
    (Maybe (Index n, a),
    Maybe (Index n), AllocatorRes n a)
allocatorStep cmd = case cmd of
    Nop -> return (Nothing, Nothing, Waiting)

    Alloc usrData -> do
        st <- get
        if freeCount st == 0 then 
            return (Nothing, Nothing, NoFreeBlocks)
        else do
            let idx = freeBlockList st !! (freeCount st - 1)
            modify (\s -> s { freeCount = freeCount s - 1})
            return (Just (idx, usrData), Nothing, AllocSuccess idx)

    Free idx -> do
        st <- get

        let isFreeAt pos =
                fromIntegral pos < freeCount st
                && (freeBlockList st !! pos) == idx
            alreadyFree = foldr (\pos rest -> isFreeAt pos || rest) False
                                (indicesI :: Vec n (Index n))
        if alreadyFree
            then return (Nothing, Nothing, InvalidFree)
            else do
                let updateFreeList = replace (freeCount st) idx (freeBlockList st)
                put st { freeBlockList = updateFreeList, freeCount = freeCount st + 1}
                return (Nothing, Nothing, Waiting)

    ReadBlockData idx -> do
        return (Nothing, Just idx, Waiting)

-- | A clocked pool allocator managing @n fixed-size blocks of payload type
-- @a, backed by a block RAM
--
-- One 'AllocatorCmd' is consumed per clock cycle. The circuit returns a pair:
--
-- ( Signal dom (AllocatorRes n a)  -- response to the command in this cycle
-- , Signal dom a                   -- block RAM data
-- )
--
-- Command behaviour:
--
-- * 'Nop' — no state change; answers 'Waiting'
--
-- * 'Alloc' @x — pops the top block from the free list, writes @x into that
--   block and answers 'AllocSuccess' with the block index. If the pool is
--   empty it answers 'NoFreeBlocks' and writes nothing
--
-- * 'Free' @i — pushes block @i back onto the free list and answers
--   'Waiting'. Freeing a block that is already free answers 'InvalidFree' and
--   leaves the state unchanged, so the free list never contains duplicates
--
-- * 'ReadBlockData' @i — presents block @i on the block RAM read port and
--   answers 'Waiting'. The data appears on the second output one clock cycle
--   later (synchronous read)
--
-- Timing and control:
--
-- * The 'AllocatorRes' is combinational with respect to the command: the
--   response to a command is visible in the same cycle
--
-- * A successful 'Alloc' drives the block RAM write port in that same cycle;
--   no extra write command or handshake is needed
--
-- * The hidden reset returns the allocator state to 'initAllocatorState', i.e
--   all @n blocks become free. The block RAM contents are not reset
--
-- * Registers and the block RAM are gated by the hidden enable line
--
-- * The block RAM is initialised with 'deepErrorX', so reading a block that has
--   never been written yields an undefined value. Freeing a block does /not/
--   clear its contents, so reading a freed block returns its last written
--   value
--
-- The state machine is implemented with 'mealy' over 'allocatorStep'

poolAllocator
    :: forall dom n a.
       (HiddenClockResetEnable dom, KnownNat n, NFDataX a)
    => Signal dom (AllocatorCmd n a)
    -> (Signal dom (AllocatorRes n a), Signal dom a)
poolAllocator cmd = (allocatorRes, blockData)
  where
    (writePort, readPort, allocatorRes) =
        unbundle (mealy step initAllocatorState cmd)

    step st c = case runState (allocatorStep c) st of
        (answer, st') -> (st', answer)

    blockData = blockRam
        (repeat (deepErrorX "poolAllocator: block was never written") :: Vec n a)
        (fmap (fromMaybe 0) readPort)
        writePort
