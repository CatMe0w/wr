module DataParser (dataParser, dataSectionParser) where

import Data.Attoparsec.ByteString
import Wasm hiding (exportIndex, offset)
import CodeParser (instructionParser)

dataParser :: Parser Data
dataParser = Data
  <$> anyWord8
  <*> many' instructionParser
  <*> takeByteString

dataSectionParser :: Parser [Data]
dataSectionParser = do
  numDataSegments <- anyWord8
  count (fromIntegral numDataSegments) dataParser
