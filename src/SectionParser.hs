module SectionParser
  ( parseSectionCode,
    parseSectionSize,
    parseSection,
  )
where

import Data.Attoparsec.ByteString
import Data.Binary (Word32)
import Data.ByteString (ByteString)
import Wasm hiding (code)

parseSectionCode :: Parser SectionCode
parseSectionCode = do
  code <- anyWord8
  case code of
    0x01 -> return TypeSection
    0x03 -> return FunctionSection
    0x0A -> return CodeSection
    _ -> return $ UnknownSection code

-- todo: parse LEB128
parseSectionSize :: Parser Word32
parseSectionSize = do
  fromIntegral <$> anyWord8

parseSection :: Parser (SectionCode, ByteString)
parseSection = do
  sectionCode <- parseSectionCode
  sectionSize <- parseSectionSize
  sectionContent <- Data.Attoparsec.ByteString.take (fromIntegral sectionSize)
  return (sectionCode, sectionContent)
