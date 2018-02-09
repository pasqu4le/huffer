# Huffer

Huffer is a small compressor and decompressor that uses [canonical Huffman codes](https://en.wikipedia.org/wiki/Canonical_Huffman_code).

I wrote this as a "toy" project to explore and learn more about the Haskell programming language.

Compiling and running
------------------------
You can use [cabal](https://www.haskell.org/cabal/) to compile and run huffer.

Compile and run with:
```
$ cabal run
```
Just compile with:
```
$ cabal configure
$ cabal build
```
Install with:
```
$ cabal install
```
Command Line Arguments
----------------------
You can see the command line arguments by running `huffer help`, that will tell you:

```
run with: huffer action [inputs] (to output)

action can be 'encode', 'decode' or 'content'
you have to specify at least one input file (or folder) to encode
you can specify only an input file to decode or list content of
you can specify an output file for encoding (or folder for decoding)
  if you don't, huffer will use 'output.huf' for encoding ('.' for decoding)
```
For example, if you run `huffer encode movies/ clips/ to vids.huf` it will compress every file contained in the _movies_ and _clips_ directories (and every one of their subdirectories) and compress all of them in a file called _vids.huf_.

You can then run `huffer content vids.huf` and it will tell you the files that _vids.huf_ contains or run `huffer decode vids.huf to media` and it will decompress every file contained in _vids.huf_ in the _media_ directory.

How files are compressed
------------------------
For each file to compress huffer will (naively) read the file and count the frequencies of every word (that has the size of a single byte).

For each file it will then calculate the Huffman code, make it canonical and finally read, compress and write them one after another.

Especially for this double-reading Huffer __it's not very fast__, but because it uses __lazy bytestrings__ it can compress files of any size in almost constant memory.

How compressed files are stored
-------------------------------
Huffer stores all the files it compressed into an archive file that starts with a __header__, structured like this:

| Bytes         | Description                               |
|:-------------:| ----------------------------------------- |
| 1             | Body type: defines how the body is stored |
| 2             | Number of entries: __n__                  |

Followed by __n__ entries (one per file), each structured like this:

| Bytes         | Description                                               |
|:-------------:| --------------------------------------------------------- |
| 4             | The size (in bytes) the file had originally               |
| 4             | The size (in bytes) the file has after compression: __m__ |
| 2             | The length of the file path: __l__                        |
| __l__         | The string containing the path of the file                |

The header is followed by the __body__ that contains for each one of the __n__ files listed in the header (in that same order) __m__ bytes of compressed data.

> NOTE: At the moment the Body Type byte is always set to 0 and not really considered because there is only one body implementation (others will follow if and when I will keep playing with this).

The body of a compressed file consists of 256 bytes, each containing the __number of bits__ for every possible word, in alphabetical order (see the [wiki page](https://en.wikipedia.org/wiki/Canonical_Huffman_code#Encoding_the_codebook) for a better explanation) followed by the actual data.
