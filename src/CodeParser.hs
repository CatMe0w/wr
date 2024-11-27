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
    0x20 -> LocalGet <$> anyWord8
    0x41 -> I32Const . fromIntegral <$> anyWord8  -- todo: parse LEB128
    0x0B -> pure End
    _ -> fail "Unknown instruction"

functionBodyParser :: Parser Function
functionBodyParser = do
  _ <- anyWord8 -- body size
  numLocals <- anyWord8
  locals <- count (fromIntegral numLocals) localParser
  code <- many' instructionParser
  return $ Function locals code

localParser :: Parser (Int, ValueType)
localParser =
  ( (,)
      . fromIntegral
      <$> anyWord8
  )
    <*> valueTypeParser

codeSectionParser :: Parser [Function]
codeSectionParser = anyWord8 >>= flip count functionBodyParser . fromIntegral
