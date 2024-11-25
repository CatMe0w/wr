module ExportParser (exportParser, exportSectionParser) where

import Data.Attoparsec.ByteString
import Wasm hiding (exportType, name)
import Prelude hiding (take)

exportParser :: Parser Export
exportParser = do
  nameLength <- anyWord8
  name <- take $ fromIntegral nameLength
  Export name <$> anyWord8 <*> anyWord8

exportSectionParser :: Parser [Export]
exportSectionParser = do
  numExports <- anyWord8
  count (fromIntegral numExports) exportParser
