module ExportParser (exportParser, exportSectionParser) where

import Data.Attoparsec.ByteString
import Wasm hiding (exportType, name)
import Prelude hiding (take)

exportParser :: Parser Export
exportParser =
  Export
    <$> (take . fromIntegral =<< anyWord8)
    <*> anyWord8
    <*> anyWord8

exportSectionParser :: Parser [Export]
exportSectionParser = anyWord8 >>= flip count exportParser . fromIntegral
