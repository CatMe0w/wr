module MemoryParser (memoryParser, memorySectionParser) where

import Data.Attoparsec.ByteString
import Wasm
import Control.Applicative (optional)

memoryParser :: Parser Memory
memoryParser = Memory
  <$> anyWord8
  <*> (flagToMax <$> anyWord8 <*> optional anyWord8)
  where
    flagToMax flag max' = if flag == 0 then Nothing else max'

memorySectionParser :: Parser [Memory]
memorySectionParser = do
  numMemories <- anyWord8
  count (fromIntegral numMemories) memoryParser