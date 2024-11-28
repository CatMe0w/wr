module DataParser (parseDataSection) where

import CodeParser (parseInstruction)
import Data.Attoparsec.ByteString
import LEB128Parser
import Wasm hiding (exportIndex, offset)

parseData :: Parser Data
parseData =
  Data
    <$> parseU32
    <*> many' parseInstruction
    <* anyWord8 -- end marker
    <*> takeByteString

parseDataSection :: Parser [Data]
parseDataSection = parseU32 >>= flip count parseData . fromIntegral
