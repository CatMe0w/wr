module DataParser (parseDataSection) where

import CodeParser (parseInstruction)
import Data.Attoparsec.ByteString
import LEB128Parser
import Wasm hiding (exportIndex, offset)

parseData :: Parser Data
parseData =
  Data
    <$> parseU32
    <*> ( (++)
            <$> manyTill parseInstruction (word8 0x0B)
            <*> pure [End]
        )
    <* parseU32
    <*> takeByteString

parseDataSection :: Parser [Data]
parseDataSection = parseU32 >>= flip count parseData . fromIntegral
