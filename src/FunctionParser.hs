module FunctionParser (functionSectionParser) where

import Data.Attoparsec.ByteString
import Data.Binary (Word8)

-- todo: parse LEB128
functionSectionParser :: Parser [Word8]
functionSectionParser = anyWord8 >>= flip count anyWord8 . fromIntegral
