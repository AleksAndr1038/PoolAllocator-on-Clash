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


--Example--
type Connection (portsNumber :: Nat) = Maybe (Port portsNumber)
type AddressBitSize = 4
type AddressNumber = Unsigned AddressBitSize

data IdOfPort (portsNumber :: Nat) = Id (Index portsNumber) | Primary
  deriving (Generic, Show, Eq, NFDataX, ShowX)

data Port (portsNumber :: Nat) = Port
  { _nodeAddress :: AddressNumber
  , _portConnectedToId :: IdOfPort portsNumber
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

data Node portsNumber agentType = Node
  { _primaryPort :: Connection portsNumber
  , _secondaryPorts :: Vec portsNumber (Maybe (Connection portsNumber))
  , _nodeType :: agentType
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

type ExamplePorts = 2
type ExampleAgent = BitVector 8

exampleNodeA :: Node ExamplePorts ExampleAgent
exampleNodeA = Node
    { _primaryPort = Just (Port 0x1 (Id 0))
    , _secondaryPorts =
           Just (Just (Port 0x2 Primary))
        :> Just Nothing
        :> Nil
    , _nodeType = 0xAA
    }

exampleNodeB :: Node ExamplePorts ExampleAgent
exampleNodeB = Node
    { _primaryPort = Nothing
    , _secondaryPorts =
           Nothing
        :> Just (Just (Port 0x3 (Id 1)))
        :> Nil
    , _nodeType = 0xBB
    }

nodeProgram :: Vec 16 (AllocatorCmd 4 (Node ExamplePorts ExampleAgent))
nodeProgram =
       Alloc exampleNodeA     -- block 3
    :> Alloc exampleNodeB     -- block 2
    :> ReadBlockData 3        -- read node A
    :> Nop                    -- data output: A
    :> ReadBlockData 2        -- read node B
    :> Nop                    -- data output: B
    :> Free 2                 -- free B
    :> Free 3                 -- free A
    :> Nop :> Nop :> Nop :> Nop
    :> Nop :> Nop :> Nop :> Nop
    :> Nil

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> ( Signal System (AllocatorRes 4 (Node ExamplePorts ExampleAgent))
       , Signal System (Node ExamplePorts ExampleAgent) )
topEntity clk rst en = withClockResetEnable clk rst en $
    let pc = register (0 :: Index 16) (satSucc SatWrap pc)
        cmd = (nodeProgram !!) <$> pc
        (res, dat) = poolAllocator @System cmd
    in (res, dat)
