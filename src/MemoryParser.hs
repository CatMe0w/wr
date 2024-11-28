module MemoryParser (parseMemorySection) where

import Control.Applicative (optional)
import Data.Attoparsec.ByteString
import LEB128Parser
import Wasm

parseMemory :: Parser Memory
parseMemory =
  Memory
    <$> parseU32
    <*> (flagToMax <$> anyWord8 <*> optional parseU32)
  where
    flagToMax flag max' = if flag == 0 then Nothing else max'

parseMemorySection :: Parser [Memory]
parseMemorySection = parseU32 >>= flip count parseMemory . fromIntegral
