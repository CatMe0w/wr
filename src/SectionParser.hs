module SectionParser
  ( parseSectionType,
    parseSectionSize,
    parseSection,
  )
where

import Data.Attoparsec.ByteString
import Data.Binary (Word32)
import Data.ByteString (ByteString)
import LEB128Parser (parseU32)
import Wasm hiding (code)

parseSectionType :: Parser SectionType
parseSectionType = do
  code <- anyWord8
  case code of
    0x01 -> return TypeSection
    0x03 -> return FunctionSection
    0x0A -> return CodeSection
    0x05 -> return MemorySection
    0x0B -> return DataSection
    0x07 -> return ExportSection
    0x02 -> return ImportSection
    _ -> return $ UnknownSection code

parseSectionSize :: Parser Word32
parseSectionSize = parseU32

parseSection :: Parser (SectionType, ByteString)
parseSection = do
  sectionType <- parseSectionType
  sectionSize <- parseSectionSize
  sectionContent <- Data.Attoparsec.ByteString.take (fromIntegral sectionSize)
  return (sectionType, sectionContent)
