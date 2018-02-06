module Encoder (encode) where

import BitUtils
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BLB
import Data.Binary.Put
import Data.Int
import Data.Word (Word8)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as SIntMap
import Data.List

data HuffTree = Leaf Int Int | Node Int HuffTree HuffTree deriving Show
data FileEntry = FileEntry {
  origSize :: Int32,
  comprSize :: Int32,
  pathSize :: Int16,
  path :: FilePath
  } deriving (Show)
type FileInfo = (FileEntry, IntMap Int)

encode :: ([FilePath], FilePath) -> IO ()
encode (inFiles, outFile) = flatList inFiles >>= writeEncoding outFile

{- IO functions to encode and write the results -}

writeEncoding :: FilePath -> [FilePath] -> IO ()
writeEncoding outFile inFiles = do
  putStrLn "\nReading frequences and writing header"
  filesInfo <- mapM generateInfo inFiles
  BL.writeFile outFile . runPut . putHeader $ map fst filesInfo
  putStrLn "\nStarting compression"
  mapM_ (compress outFile) filesInfo
  putStrLn "\nFinished"

compress :: FilePath -> FileInfo -> IO ()
compress outFile (fileEntry, sizesMap) = do
  let filePath = path fileEntry
      codebook = makeCodebook sizesMap
      encoder = getHuffEncoder $ IntMap.toList sizesMap
      putComp = putCompression codebook encoder
  putStrLn $ "Compressing: " ++ filePath
  BL.readFile filePath >>= BL.appendFile outFile . runPut . putComp . BL.unpack

{- Put functions used by the IO functions -}

putHeader :: [FileEntry] -> Put
putHeader entries = do
  putWord8 0 {- flag to be used in the future -}
  putNum entries
  mapM_ putEntry entries

putNum :: [FileEntry] -> Put
putNum = putInt16be . fromIntegral . length

putEntry :: FileEntry -> Put
putEntry fileEntry = do
  putInt32be $ origSize fileEntry
  putInt32be $ comprSize fileEntry
  putInt16be $ pathSize fileEntry
  putLazyByteString . BLB.toLazyByteString . BLB.string8 $ path fileEntry

putCompression :: [Int] -> ([Word8] -> [Word8]) -> [Word8] -> Put
putCompression codebook encoder input = do
  putCodebook codebook
  mapM_ putWord8 $ encoder input

putCodebook :: [Int] -> Put
putCodebook = mapM_ (putWord8 . fromIntegral)

{- Functions to generate info required by the Put functions -}

generateInfo :: FilePath -> IO FileInfo
generateInfo filePath = do
  fileSize <- getFileSize filePath
  freqs <- readFrequences filePath
  putStrLn $ "reading: " ++ filePath
  let sizesMap = codesSize 0 $ treeFromFreqs freqs
      compSize = 256 + compressedSize freqs sizesMap
      fileEntry = FileEntry {origSize = fromInteger fileSize,
                            comprSize = fromIntegral compSize,
                            pathSize = fromIntegral $ length filePath,
                            path = filePath}
  return (fileEntry, sizesMap)

makeCodebook :: IntMap Int -> [Int]
makeCodebook sizesMap = map (codebookLookup sizesMap) [0..255]

codebookLookup :: IntMap Int -> Int -> Int
codebookLookup sizesMap n = IntMap.findWithDefault 0 n sizesMap

compressedSize :: IntMap Int -> IntMap Int -> Int
compressedSize freqs sizes = bytesNum + ceil
  where
    bitsNum = sum . IntMap.elems $ IntMap.unionWith (*) freqs sizes
    bytesNum = div bitsNum 8
    ceil = if mod bitsNum 8 /= 0 then 1 else 0

{- Maps every byte to the bit-lenght of it's encoding from the Huffman tree -}

codesSize :: Int -> HuffTree -> IntMap Int
codesSize d (Leaf val _) = IntMap.singleton val d
codesSize d (Node _ l r) = IntMap.union (codesSize (d+1) l) (codesSize (d+1) r)

{- Functions to make a Huffman tree from a map of bytes frequences -}

treeFromFreqs :: IntMap Int -> HuffTree
treeFromFreqs = findTree . map toLeaf . sortOn snd . IntMap.toList

findTree :: [HuffTree] -> HuffTree
findTree [] = Leaf 0 0
findTree [x] = x
findTree (a:b:lst) = findTree . priorityInsert lst $ mergeCouple (a,b)

mergeCouple :: (HuffTree, HuffTree) -> HuffTree
mergeCouple (l, r) = Node (treeFreq l + treeFreq r) l r

priorityInsert :: [HuffTree] -> HuffTree -> [HuffTree]
priorityInsert [] tree = [tree]
priorityInsert (x:xs) tree
  | treeFreq x < treeFreq tree = x : priorityInsert xs tree
  | otherwise = tree : x : xs

treeFreq :: HuffTree -> Int
treeFreq (Leaf _ freq) = freq
treeFreq (Node freq _ _) = freq

toLeaf :: (Int, Int) -> HuffTree
toLeaf (val, freq) = Leaf val freq

{- Functions to read the frequence of each byte in a file -}
{- NOTE: uses strict IntMaps and foldl' to run in constant memory -}

readFrequences :: FilePath -> IO (IntMap Int)
readFrequences = (BL.foldl' mapFrequences SIntMap.empty <$>) . BL.readFile

mapFrequences :: SIntMap.IntMap Int -> Word8 -> SIntMap.IntMap Int
mapFrequences fr n = SIntMap.insertWith (+) (fromIntegral n) 1 fr

{- To unfold a list of files/directories into a flat list of existing files -}

flatList :: [FilePath] -> IO [FilePath]
flatList [] = return []
flatList (filePath:paths) = do
  isDir <- doesDirectoryExist filePath
  isFile <- doesFileExist filePath
  following <- flatList paths
  if isDir then do
    children <- listDirectory filePath
    flatChildren <- flatList $ map (filePath </>) children
    return $ following ++ flatChildren
  else if isFile then return $ filePath : following
  else do
    putStrLn $ "Warning: " ++ filePath ++ " is not a file. Skipped."
    return following

{- Functions that work with lists of bits (big-endian) -}

getHuffEncoder :: [(Int, Int)] -> ([Word8] -> [Word8])
getHuffEncoder = huffEncoder . IntMap.fromList . canonicalList

huffEncoder :: IntMap BitList -> [Word8] -> [Word8]
huffEncoder cMap = wrdsFromBits . concatMap (wrdMapper cMap)

wrdMapper :: IntMap BitList -> Word8 -> BitList
wrdMapper cMap x = lst
  where (Just lst) = IntMap.lookup (fromIntegral x) cMap
