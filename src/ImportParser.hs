module ImportParser (importParser, importSectionParser) where

import Data.Attoparsec.ByteString
import Wasm hiding (name)
import Prelude hiding (take)

importParser :: Parser Import
importParser = do
  moduleLength <- anyWord8
  module' <- take $ fromIntegral moduleLength
  nameLength <- anyWord8
  name <- take $ fromIntegral nameLength
  Import module' name <$> anyWord8

importSectionParser :: Parser [Import]
importSectionParser = do
  numImports <- anyWord8
  count (fromIntegral numImports) importParser