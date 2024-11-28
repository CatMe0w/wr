module FunctionParser (functionSectionParser) where

import Data.Attoparsec.ByteString
import Data.Binary (Word32)
import LEB128Parser

functionSectionParser :: Parser [Word32]
functionSectionParser = anyWord8 >>= flip count parseU32 . fromIntegral
