module CodeParser
  ( instructionParser,
    functionBodyParser,
    localParser,
    codeSectionParser,
  )
where

import Control.Monad (replicateM)
import Data.Attoparsec.ByteString
import Data.Binary (Word8)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import TypeParser (valueTypeParser)
import Wasm hiding (code, locals)
import Data.Attoparsec.Binary (anyWord32le, anyWord64le)

instructionParser :: Parser Instruction
instructionParser = do
  -- todo: parse LEB128
  opcode <- anyWord8
  case opcode of
    0x00 -> pure Unreachable
    0x01 -> pure Nop
    -- 0x02 -> Block . blockTypeParser <$> anyWord8
    -- 0x03 -> Loop . blockTypeParser <$> anyWord8
    -- 0x04 -> If . blockTypeParser <$> anyWord8
    -- 0x05 -> pure Else
    0x0B -> pure End
    0x0C -> Br . fromIntegral <$> anyWord8
    0x0D -> BrIf . fromIntegral <$> anyWord8
    0x0E -> do
      count' <- fromIntegral <$> anyWord8
      labels <- replicateM count' (fromIntegral <$> anyWord8)
      defaultLabel <- fromIntegral <$> anyWord8
      return $ BrTable labels defaultLabel
    0x0F -> pure Return
    0x10 -> Call . fromIntegral <$> anyWord8
    0x11 -> CallIndirect . fromIntegral <$> anyWord8
    0x1A -> pure Drop
    0x1B -> pure Select
    0x20 -> LocalGet . fromIntegral <$> anyWord8
    0x21 -> LocalSet . fromIntegral <$> anyWord8
    0x22 -> LocalTee . fromIntegral <$> anyWord8
    0x23 -> GlobalGet . fromIntegral <$> anyWord8
    0x24 -> GlobalSet . fromIntegral <$> anyWord8
    0x28 -> pure I32Load
    0x29 -> pure I64Load
    0x2A -> pure F32Load
    0x2B -> pure F64Load
    0x2C -> pure I32Load8S
    0x2D -> pure I32Load8U
    0x2E -> pure I32Load16S
    0x2F -> pure I32Load16U
    0x30 -> pure I64Load8S
    0x31 -> pure I64Load8U
    0x32 -> pure I64Load16S
    0x33 -> pure I64Load16U
    0x34 -> pure I64Load32S
    0x35 -> pure I64Load32U
    0x36 -> pure I32Store
    0x37 -> pure I64Store
    0x38 -> pure F32Store
    0x39 -> pure F64Store
    0x3A -> pure I32Store8
    0x3B -> pure I32Store16
    0x3C -> pure I64Store8
    0x3D -> pure I64Store16
    0x3E -> pure I64Store32
    0x3F -> pure MemorySize
    0x40 -> pure MemoryGrow
    0x41 -> I32Const . fromIntegral <$> anyWord8
    0x42 -> I64Const . fromIntegral <$> anyWord8
    0x43 -> F32Const . wordToFloat <$> anyWord32le
    0x44 -> F64Const . wordToDouble <$> anyWord64le
    0x45 -> pure I32Eqz
    0x46 -> pure I32Eq
    0x47 -> pure I32Ne
    0x48 -> pure I32LtS
    0x49 -> pure I32LtU
    0x4A -> pure I32GtS
    0x4B -> pure I32GtU
    0x4C -> pure I32LeS
    0x4D -> pure I32LeU
    0x4E -> pure I32GeS
    0x4F -> pure I32GeU
    0x50 -> pure I64Eqz
    0x51 -> pure I64Eq
    0x52 -> pure I64Ne
    0x53 -> pure I64LtS
    0x54 -> pure I64LtU
    0x55 -> pure I64GtS
    0x56 -> pure I64GtU
    0x57 -> pure I64LeS
    0x58 -> pure I64LeU
    0x59 -> pure I64GeS
    0x5A -> pure I64GeU
    0x5B -> pure F32Eq
    0x5C -> pure F32Ne
    0x5D -> pure F32Lt
    0x5E -> pure F32Gt
    0x5F -> pure F32Le
    0x60 -> pure F32Ge
    0x61 -> pure F64Eq
    0x62 -> pure F64Ne
    0x63 -> pure F64Lt
    0x64 -> pure F64Gt
    0x65 -> pure F64Le
    0x66 -> pure F64Ge
    0x67 -> pure I32Clz
    0x68 -> pure I32Ctz
    0x69 -> pure I32Popcnt
    0x6A -> pure I32Add
    0x6B -> pure I32Sub
    0x6C -> pure I32Mul
    0x6D -> pure I32DivS
    0x6E -> pure I32DivU
    0x6F -> pure I32RemS
    0x70 -> pure I32RemU
    0x71 -> pure I32And
    0x72 -> pure I32Or
    0x73 -> pure I32Xor
    0x74 -> pure I32Shl
    0x75 -> pure I32ShrS
    0x76 -> pure I32ShrU
    0x77 -> pure I32Rotl
    0x78 -> pure I32Rotr
    0x79 -> pure I64Clz
    0x7A -> pure I64Ctz
    0x7B -> pure I64Popcnt
    0x7C -> pure I64Add
    0x7D -> pure I64Sub
    0x7E -> pure I64Mul
    0x7F -> pure I64DivS
    0x80 -> pure I64DivU
    0x81 -> pure I64RemS
    0x82 -> pure I64RemU
    0x83 -> pure I64And
    0x84 -> pure I64Or
    0x85 -> pure I64Xor
    0x86 -> pure I64Shl
    0x87 -> pure I64ShrS
    0x88 -> pure I64ShrU
    0x89 -> pure I64Rotl
    0x8A -> pure I64Rotr
    0x8B -> pure F32Abs
    0x8C -> pure F32Neg
    0x8D -> pure F32Ceil
    0x8E -> pure F32Floor
    0x8F -> pure F32Trunc
    0x90 -> pure F32Nearest
    0x91 -> pure F32Sqrt
    0x92 -> pure F32Add
    0x93 -> pure F32Sub
    0x94 -> pure F32Mul
    0x95 -> pure F32Div
    0x96 -> pure F32Min
    0x97 -> pure F32Max
    0x98 -> pure F32Copysign
    0x99 -> pure F64Abs
    0x9A -> pure F64Neg
    0x9B -> pure F64Ceil
    0x9C -> pure F64Floor
    0x9D -> pure F64Trunc
    0x9E -> pure F64Nearest
    0x9F -> pure F64Sqrt
    0xA0 -> pure F64Add
    0xA1 -> pure F64Sub
    0xA2 -> pure F64Mul
    0xA3 -> pure F64Div
    0xA4 -> pure F64Min
    0xA5 -> pure F64Max
    0xA6 -> pure F64Copysign
    0xA7 -> pure I32WrapI64
    0xA8 -> pure I32TruncF32S
    0xA9 -> pure I32TruncF32U
    0xAA -> pure I32TruncF64S
    0xAB -> pure I32TruncF64U
    0xAC -> pure I64ExtendI32S
    0xAD -> pure I64ExtendI32U
    0xAE -> pure I64TruncF32S
    0xAF -> pure I64TruncF32U
    0xB0 -> pure I64TruncF64S
    0xB1 -> pure I64TruncF64U
    0xB2 -> pure F32ConvertI32S
    0xB3 -> pure F32ConvertI32U
    0xB4 -> pure F32ConvertI64S
    0xB5 -> pure F32ConvertI64U
    0xB6 -> pure F32DemoteF64
    0xB7 -> pure F64ConvertI32S
    0xB8 -> pure F64ConvertI32U
    0xB9 -> pure F64ConvertI64S
    0xBA -> pure F64ConvertI64U
    0xBB -> pure F64PromoteF32
    0xBC -> pure I32ReinterpretF32
    0xBD -> pure I64ReinterpretF64
    0xBE -> pure F32ReinterpretI32
    0xBF -> pure F64ReinterpretI64
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
