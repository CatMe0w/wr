module ImportParser (parseImportSection) where

import Data.Attoparsec.ByteString
import LEB128Parser
import Wasm hiding (name)
import Prelude hiding (take)

parseImport :: Parser Import
parseImport =
  Import
    <$> (take . fromIntegral =<< parseU32)
    <*> (take . fromIntegral =<< parseU32)
    <*> anyWord8

parseImportSection :: Parser [Import]
parseImportSection = parseU32 >>= flip count parseImport . fromIntegral
