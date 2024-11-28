module DataParser (parseDataSection) where

import CodeParser (parseInstruction)
import Data.Attoparsec.ByteString
import Wasm hiding (exportIndex, offset)

parseData :: Parser Data
parseData =
  Data
    <$> anyWord8
    <*> many' parseInstruction
    <* anyWord8
    <*> takeByteString

parseDataSection :: Parser [Data]
parseDataSection = anyWord8 >>= flip count parseData . fromIntegral
