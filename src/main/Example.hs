{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}


module Example where

import PoolAllocator
import Clash.Prelude


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
