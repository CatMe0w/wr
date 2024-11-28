module ImportParser (parseImportSection) where

import Data.Attoparsec.ByteString
import Wasm hiding (name)
import Prelude hiding (take)

parseImport :: Parser Import
parseImport =
  Import
    <$> (take . fromIntegral =<< anyWord8)
    <*> (take . fromIntegral =<< anyWord8)
    <*> anyWord8

parseImportSection :: Parser [Import]
parseImportSection = anyWord8 >>= flip count parseImport . fromIntegral
