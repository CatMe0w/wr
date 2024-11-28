module FunctionParser (parseFunctionSection) where

import Data.Attoparsec.ByteString
import Data.Binary (Word32)
import LEB128Parser

parseFunctionSection :: Parser [Word32]
parseFunctionSection = anyWord8 >>= flip count parseU32 . fromIntegral
