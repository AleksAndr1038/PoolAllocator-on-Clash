{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PoolAllocator where

import Clash.Prelude
import Control.Monad.State

type BlockSize = BitVector 512

data AllocatorCmd n a = Nop | Alloc a | Free (Index n) | ReadBlockData (Index n)
    deriving (Eq, Show)

data AllocatorRes n a = Waiting | AllocSuccess (Index n) | NoFreeBlocks
    deriving (Eq, Show)

data AllocatorState n = AllocatorState {
    freeBlockList :: Vec n (Index n),
    freeCount :: Index (n + 1)
} deriving (Eq, Show)

initAllocatorState :: (KnownNat n) => AllocatorState n
initAllocatorState = AllocatorState {
    freeBlockList = indicesI,
    freeCount = maxBound
}

allocatorStep
    :: (KnownNat n, NFDataX a)
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
        let updateFreeList = replace (freeCount st) idx (freeBlockList st)
        put st { freeBlockList = updateFreeList, freeCount = freeCount st + 1}
        return (Nothing, Nothing, Waiting)

    ReadBlockData idx -> do
        return (Nothing, Just idx, Waiting)
