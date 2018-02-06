module Main where

import Encoder (encode)
import Decoder (decode, content)
import System.Environment

main :: IO ()
main = getArgs >>= execute

execute :: [String] -> IO ()
execute ("encode":args) = parseAndRun encode args "output.huf"
execute ("decode":args) = parseAndRun decode args "."
execute ["content",fileName] = content fileName
execute ("help":_) = mapM_ putStrLn helpLines
execute [] = showInvalid "No action specified"
execute (action:_) = showInvalid $ "Unknown action: " ++ action

parseAndRun :: (([String], String) -> IO()) -> [String] -> String -> IO()
parseAndRun action args defaultOut = case span (/="to") args of
  ([], _) -> showInvalid "No input file specified"
  (inputs, []) -> action (inputs, defaultOut)
  (inputs, ["to", out]) -> action (inputs, out)
  _ -> showInvalid "You can specify one output file"

helpLines :: [String]
helpLines = [
  "huffer is a compressor and decompressor based on canonical Huffman coding\n",
  "run with: huffer action [inputs] (to output)\n",
  "action can be 'encode', 'decode' or 'content'",
  "you have to specify at least one input file (or folder) to encode",
  "you can specify only an input file to decode or content",
  "you can specify an output file for encoding (or folder for decoding)",
  "  if you don't, huffer will use 'output.huf' for encoding ('.' for decoding)"
  ]

showInvalid :: String ->  IO ()
showInvalid err = mapM_ putStrLn ["Invalid input!", err,
                                  "run: 'huffer help' for help"]
