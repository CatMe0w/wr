module ImportParser (importParser, importSectionParser) where

import Data.Attoparsec.ByteString
import Wasm hiding (name)
import Prelude hiding (take)

importParser :: Parser Import
importParser = 
  Import
    <$> (take . fromIntegral =<< anyWord8)
    <*> (take . fromIntegral =<< anyWord8)
    <*> anyWord8

importSectionParser :: Parser [Import]
importSectionParser = anyWord8 >>= flip count importParser . fromIntegral
