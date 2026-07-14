# PoolAllocator-on-Clash

This repository contains pool allocator implementation on Clash


## API Reference

#### createPool

```haskell
  createPool :: KnownNat numBlocks => Int -> PoolAllocator numBlocks
```

| Parameter | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `blockSize` | `Int` | Size of one memory block |
| `numBlocks` | `Int` | pool block count parameter |

#### allocate

```haskell
  allocate :: KnownNat numBlocks => PoolAllocator numBlocks -> (PoolAllocator numBlocks, Maybe (Index numBlocks))
```

| Parameter | Type     | Description                       |
| :-------- | :------- | :-------------------------------- |
| `alloc`      | `PoolAllocator numBlocks` | parameter describing the pool allocator |

#### deallocate

```haskell
  deallocate :: KnownNat numBlocks => PoolAllocator numBlocks -> Index numBlocks -> PoolAllocator numBlocks
```

| Parameter | Type     | Description                       |
| :-------- | :------- | :-------------------------------- |
| `dealloc` | `PoolAllocator numBlocks` | parameter describing the pool allocator |
| `idx`      | `Index numBlocks` | index of the block to be freed |



## Usage/Examples

```haskell
import PoolAllocator
import Clash.Prelude

topEntity :: ([Index 4], Vec 4 Block, Maybe (Index 4), Maybe (Index 4), Maybe (Index 4))
topEntity = (freeList a4, pool a4, i1, i2, i3) where
    a0 = createPool 16

    (a1, i1) = allocate a0
    (a2, i2) = allocate a1

    a3 = case i1 of
        Nothing -> a2
        Just idx -> deallocate a2 idx

    (a4, i3) = allocate a3

-- Output:
--([2,3],Allocated :> Allocated :> Free :> Free :> Nil,Just 0,Just 1,Just 0)
```
