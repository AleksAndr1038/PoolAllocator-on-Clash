module PoolAllocator where

import Clash.Prelude

data Block = Free | Allocated deriving Show

data PoolAllocator (numBlocks :: Nat) = PoolAllocator {
    blockSize :: Int,
    freeList :: [Index numBlocks],
    pool :: Vec numBlocks Block
}

createPool :: KnownNat numBlocks => Int -> PoolAllocator numBlocks
createPool blockSize' = PoolAllocator {
    blockSize = blockSize',
    freeList = [minBound..maxBound],
    pool = repeat Free
}

allocate :: KnownNat numBlocks => PoolAllocator  numBlocks -> (PoolAllocator numBlocks, Maybe (Index numBlocks))
allocate alloc = case freeList alloc of
    [] -> (alloc, Nothing)
    idx : free -> (alloc {
        freeList = free,
        pool = replace idx Allocated (pool alloc)}, Just idx)

deallocate :: KnownNat numBlocks => PoolAllocator numBlocks -> Index numBlocks -> PoolAllocator numBlocks
deallocate dealloc idx = dealloc {
    freeList = idx : freeList dealloc,
    pool = replace idx Free (pool dealloc)}

topEntity :: ([Index 4], Vec 4 Block, Maybe (Index 4), Maybe (Index 4), Maybe (Index 4))
topEntity = (freeList a4, pool a4, i1, i2, i3) where
    a0 = createPool 16

    (a1, i1) = allocate a0
    (a2, i2) = allocate a1

    a3 = case i1 of
        Nothing -> a2
        Just idx -> deallocate a2 idx

    (a4, i3) = allocate a3
