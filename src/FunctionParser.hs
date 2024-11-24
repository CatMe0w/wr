module FunctionParser (functionSectionParser) where

import Data.Attoparsec.ByteString
import Data.Binary (Word8)

-- todo: parse LEB128
functionSectionParser :: Parser [Word8]
functionSectionParser = do
  numFunctions <- anyWord8
  count (fromIntegral numFunctions) anyWord8
