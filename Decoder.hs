module Decoder (decode, content) where

import BitUtils
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BLC
import System.FilePath
import System.Directory
import Data.Int
import Data.Binary.Get
import Control.Monad
import Data.List
import Data.Word (Word8)

data HuffTree = Leaf Word8 | Node HuffTree HuffTree deriving Show
type FileData = (FileEntry, BL.ByteString, BL.ByteString)
data FileEntry = FileEntry {
  origSize :: Int32,
  comprSize :: Int32,
  path :: FilePath
  }
instance Show FileEntry where
  show (FileEntry o c n) = intercalate "\t" [show o, show c, n]

{- IO functions to decode and display content -}

decode :: ([FilePath], FilePath) -> IO ()
decode (inFiles, outDir) = mapM_ (extract outDir) inFiles

extract :: FilePath -> FilePath -> IO ()
extract outDir filePath = do
  isFile <- doesFileExist filePath
  if isFile then do
    putStrLn $ "Reading " ++ filePath
    (entryNum, filesData) <- readData filePath
    putStrLn $ show entryNum ++ " files to extract\n"
    mapM_ (decompress outDir) filesData
    putStrLn "\nDone.\n"
  else
    putStrLn $ "Warning: " ++ filePath ++ " is not a file. Skipped."

decompress :: FilePath -> FileData -> IO ()
decompress outDir (fileEntry, codebook, comprData) = do
  let outPath = outDir </> path fileEntry
      sizesList = zip [0..255] . map fromIntegral $ BL.unpack codebook
      decoder = getHuffDecoder (fromIntegral $ origSize fileEntry) sizesList
  putStrLn $ "Decompressing: " ++ outPath
  createDirectoryIfMissing True $ takeDirectory outPath
  BL.writeFile outPath . BL.pack . decoder $ BL.unpack comprData

readData :: FilePath -> IO (Int,[FileData])
readData filePath = do
  cont <- BL.readFile filePath
  return $ runGet getData cont

content :: FilePath -> IO ()
content filePath = do
  isFile <- doesFileExist filePath
  if isFile then
    BL.readFile filePath >>= printContent filePath . runGet getHeader
  else
    putStrLn $ "Error: " ++ filePath ++ " is not a file."

printContent :: FilePath -> (Int, [FileEntry]) -> IO ()
printContent filePath (entryNum, entries) = do
  putStrLn $ concat [filePath, " contains ", show entryNum, " files:\n"]
  putStrLn "Original\tCompressed\tFileName"
  putStrLn "Size (bytes)\tSize (bytes)\n"
  mapM_ print entries
  putStrLn $ "\nTotal original bytes:\t" ++ (show . sum $ map origSize entries)
  putStrLn $ "Total compressed bytes:\t" ++ (show . sum $ map comprSize entries)

{- Get functions used by the IO functions -}

getHeader :: Get (Int,[FileEntry])
getHeader = do
  _ <- getWord8 {- flag to be used in the future -}
  entryNum <- getInt16be
  entries <- replicateM (fromIntegral entryNum) getEntry
  return (fromIntegral entryNum, entries)

getData :: Get (Int,[FileData])
getData = do
  (entryNum, entries) <- getHeader
  body <- mapM getBody entries
  return (entryNum, body)

getBody :: FileEntry -> Get FileData
getBody fileEntry = do
  codebook <- getLazyByteString 256
  comprData <- getLazyByteString . fromIntegral $ comprSize fileEntry - 256
  return (fileEntry, codebook, comprData)

getEntry :: Get FileEntry
getEntry = do
  oSize <- getInt32be
  cSize <- getInt32be
  pathSize <- getInt16be
  filePath <- getByteString $ fromIntegral pathSize
  return FileEntry {origSize = fromIntegral oSize,
                    comprSize = fromIntegral cSize,
                    path = BLC.unpack filePath}

{- Functions that work with lists of bits (big-endian) -}

getHuffDecoder :: Int -> [(Word8, Int)] -> ([Word8] -> [Word8])
getHuffDecoder num = huffDecoder num . treeFromBits . canonicalList . filter nz

nz :: (Word8, Int) -> Bool
nz (_, v) = v /= 0

treeFromBits :: [(Word8, BitList)] -> HuffTree
treeFromBits [(w,[])] = Leaf w
treeFromBits lst = Node (treeFromBits zeros) (treeFromBits ones)
  where
    (ons, zrs) = partition (head . snd) lst
    (ones, zeros) = (map bitsTail ons, map bitsTail zrs)

bitsTail :: (Word8, BitList) -> (Word8, BitList)
bitsTail (w, []) = (w, [])
bitsTail (w, _:xs) = (w, xs)

huffDecoder :: Int -> HuffTree -> [Word8] -> [Word8]
huffDecoder num huffTree = traverseTree num huffTree huffTree . wrdsToBits

traverseTree :: Int -> HuffTree -> HuffTree -> BitList -> [Word8]
traverseTree 0 _ _ _ = []
traverseTree _ _ _ [] = []
traverseTree n root (Leaf w) lst = w : traverseTree (n-1) root root lst
traverseTree n root (Node l _) (False:lst) = traverseTree n root l lst
traverseTree n root (Node _ r) (True:lst) = traverseTree n root r lst
