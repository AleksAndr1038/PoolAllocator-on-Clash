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
