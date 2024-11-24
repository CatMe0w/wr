module CodeParser
  ( instructionParser,
    functionBodyParser,
    localParser,
    codeSectionParser,
  )
where

import Data.Attoparsec.ByteString
import TypeParser (valueTypeParser)
import Wasm hiding (code, locals)

instructionParser :: Parser Instruction
instructionParser = do
  opcode <- anyWord8
  case opcode of
    0x20 -> do
      LocalGet <$> anyWord8
    0x0B -> return End
    _ -> fail "Unknown instruction"

functionBodyParser :: Parser Function
functionBodyParser = do
  _ <- anyWord8 -- body size
  numLocals <- anyWord8
  locals <- count (fromIntegral numLocals) localParser
  code <- many' instructionParser
  return $ Function locals code

localParser :: Parser (Int, ValueType)
localParser = do
  _count <- anyWord8
  valueType <- valueTypeParser
  return (fromIntegral _count, valueType)

codeSectionParser :: Parser [Function]
codeSectionParser = do
  numFunctions <- anyWord8
  count (fromIntegral numFunctions) functionBodyParser
