module ExportParser (parseExport, parseExportSection) where

import Data.Attoparsec.ByteString
import Wasm hiding (exportType, name)
import Prelude hiding (take)

parseExport :: Parser Export
parseExport =
  Export
    <$> (take . fromIntegral =<< anyWord8)
    <*> anyWord8
    <*> anyWord8

parseExportSection :: Parser [Export]
parseExportSection = anyWord8 >>= flip count parseExport . fromIntegral
