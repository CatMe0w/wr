module DataParser (dataParser, dataSectionParser) where

import CodeParser (instructionParser)
import Data.Attoparsec.ByteString
import Wasm hiding (exportIndex, offset)

dataParser :: Parser Data
dataParser =
  Data
    <$> anyWord8
    <*> many' instructionParser
    <* anyWord8
    <*> takeByteString

dataSectionParser :: Parser [Data]
dataSectionParser = anyWord8 >>= flip count dataParser . fromIntegral
