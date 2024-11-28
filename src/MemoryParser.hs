module MemoryParser (parseMemory, parseMemorySection) where

import Control.Applicative (optional)
import Data.Attoparsec.ByteString
import Wasm

parseMemory :: Parser Memory
parseMemory =
  Memory
    <$> anyWord8
    <*> (flagToMax <$> anyWord8 <*> optional anyWord8)
  where
    flagToMax flag max' = if flag == 0 then Nothing else max'

parseMemorySection :: Parser [Memory]
parseMemorySection = anyWord8 >>= flip count parseMemory . fromIntegral
