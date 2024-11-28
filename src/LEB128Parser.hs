{-# LANGUAGE FlexibleContexts #-}

module LEB128Parser
  ( parseU32,
    parseU64,
    parseI32,
    parseI64,
  )
where

import Data.Attoparsec.ByteString
import Data.Bits hiding (shift)
import Data.Int
import Data.Word

class (Integral a, Bits a) => ULEB128 a where
  maxBits :: a -> Int

instance ULEB128 Word32 where
  maxBits _ = 32

instance ULEB128 Word64 where
  maxBits _ = 64

class (Integral a, Bits a) => SLEB128 a where
  signBit :: a -> Int
  signExtend :: a -> Int -> a

instance SLEB128 Int32 where
  signBit _ = 31
  signExtend value shift = value .|. ((-1) `shiftL` shift)

instance SLEB128 Int64 where
  signBit _ = 63
  signExtend value shift = value .|. ((-1) `shiftL` shift)

parseULEB128 :: (ULEB128 a) => Parser a
parseULEB128 = go 0 0
  where
    go result shift = do
      byte <- anyWord8
      let value = result .|. (fromIntegral (byte .&. 0x7F) `shiftL` shift)
      if (byte .&. 0x80) /= 0 && shift < maxBits result
        then go value (shift + 7)
        else return value

parseSLEB128 :: (SLEB128 a) => Parser a
parseSLEB128 = go 0 0
  where
    go result shift = do
      byte <- anyWord8
      let value = result .|. (fromIntegral (byte .&. 0x7F) `shiftL` shift)
      if (byte .&. 0x80) /= 0
        then go value (shift + 7)
        else do
          if (byte .&. 0x40) /= 0 && shift < signBit result
            then return $ signExtend value (shift + 7)
            else return value

parseU32 :: Parser Word32
parseU32 = parseULEB128

parseU64 :: Parser Word64
parseU64 = parseULEB128

parseI32 :: Parser Int32
parseI32 = parseSLEB128

parseI64 :: Parser Int64
parseI64 = parseSLEB128
