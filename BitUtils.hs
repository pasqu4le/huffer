module BitUtils (
  BitList,
  emptyBitList,
  canonicalList,
  wrdsFromBits,
  wrdsToBits
) where

import Data.Bits
import Data.List
import Data.Word (Word8)

{- implements a list of bits as bools (big-endian) and some operations -}

type BitList = [Bool]

emptyBitList :: Int -> BitList
emptyBitList n = replicate n False

wrdsToBits :: [Word8] -> BitList
wrdsToBits = concatMap wrdToBits

wrdToBits :: Word8 -> BitList
wrdToBits x = map (testBit x) [7,6..0]

wrdsFromBits :: BitList -> [Word8]
wrdsFromBits [] = []
wrdsFromBits lst = wrdFromBits w : wrdsFromBits ws
  where (w,ws) = splitAt 8 lst

wrdFromBits :: BitList -> Word8
wrdFromBits = foldl' (.|.) 0 . map (bit . fst) . filter snd . zip [7,6..0]

{- helper functions to create canonical Huffman codes-}

canonicalList :: Integral a => [(a, Int)] -> [(a, BitList)]
canonicalList [] = []
canonicalList sizesList = foldl nextCanon [] . sortOn snd $ sortOn fst sizesList

nextCanon :: Integral a => [(a, BitList)] -> (a, Int) -> [(a, BitList)]
nextCanon [] (x,n) = [(x, emptyBitList n)]
nextCanon ((p,prevBits):bts) (x,n) = (x, newBits) : (p,prevBits) : bts
  where newBits = fillTo n $ increment prevBits

fillTo :: Int -> BitList -> BitList
fillTo n lst = lst ++ emptyBitList (n - length lst)

increment :: BitList -> BitList
increment lst = if lft then True:res else res
  where (res, lft) = foldr bitSum ([], True) lst

bitSum :: Bool -> (BitList, Bool) -> (BitList, Bool)
bitSum True (lst, True) = (False:lst, True)
bitSum False (lst, True) = (True:lst, False)
bitSum x (lst, False) = (x:lst, False)
