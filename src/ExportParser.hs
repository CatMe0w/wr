module ExportParser (parseExportSection) where

import Data.Attoparsec.ByteString
import LEB128Parser
import Wasm hiding (exportType, name)
import Prelude hiding (take)

parseExport :: Parser Export
parseExport =
  Export
    <$> (take . fromIntegral =<< parseU32)
    <*> anyWord8
    <*> parseU32

parseExportSection :: Parser [Export]
parseExportSection = parseU32 >>= flip count parseExport . fromIntegral
