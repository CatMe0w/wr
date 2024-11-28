module Wasm
  ( ValueType (..),
    FuncType (..),
    Function (..),
    BlockType (..),
    Instruction (..),
    Memory (..),
    Data (..),
    Export (..),
    Import (..),
    SectionType (..),
    Module (..),
  )
where

import Data.Binary (Word8)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Word (Word32)

data ValueType = I32 | I64 | F32 | F64 deriving (Show, Eq)

data FuncType = FuncType
  { params :: [ValueType],
    results :: [ValueType]
  }
  deriving (Show, Eq)

data Function = Function
  { locals :: [(Int, ValueType)],
    code :: [Instruction]
  }
  deriving (Show, Eq)

data BlockType
  = EmptyBlock
  | ValueBlock ValueType
  deriving (Show, Eq)

data Instruction
  = Unreachable
  | Nop
  | Block BlockType
  | Loop BlockType
  | If BlockType
  | Else
  | End
  | Br Word32
  | BrIf Word32
  | BrTable [Word32] Word32
  | Return
  | Call Word32
  | CallIndirect Word32
  | Drop
  | Select
  | LocalGet Word32
  | LocalSet Word32
  | LocalTee Word32
  | GlobalGet Word32
  | GlobalSet Word32
  | I32Load
  | I64Load
  | F32Load
  | F64Load
  | I32Load8S
  | I32Load8U
  | I32Load16S
  | I32Load16U
  | I64Load8S
  | I64Load8U
  | I64Load16S
  | I64Load16U
  | I64Load32S
  | I64Load32U
  | I32Store
  | I64Store
  | F32Store
  | F64Store
  | I32Store8
  | I32Store16
  | I64Store8
  | I64Store16
  | I64Store32
  | MemorySize
  | MemoryGrow
  | I32Const Int32
  | I64Const Int64
  | F32Const Float
  | F64Const Double
  | I32Eqz
  | I32Eq
  | I32Ne
  | I32LtS
  | I32LtU
  | I32GtS
  | I32GtU
  | I32LeS
  | I32LeU
  | I32GeS
  | I32GeU
  | I64Eqz
  | I64Eq
  | I64Ne
  | I64LtS
  | I64LtU
  | I64GtS
  | I64GtU
  | I64LeS
  | I64LeU
  | I64GeS
  | I64GeU
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt
  | F32Le
  | F32Ge
  | F64Eq
  | F64Ne
  | F64Lt
  | F64Gt
  | F64Le
  | F64Ge
  | I32Clz
  | I32Ctz
  | I32Popcnt
  | I32Add
  | I32Sub
  | I32Mul
  | I32DivS
  | I32DivU
  | I32RemS
  | I32RemU
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32ShrS
  | I32ShrU
  | I32Rotl
  | I32Rotr
  | I64Clz
  | I64Ctz
  | I64Popcnt
  | I64Add
  | I64Sub
  | I64Mul
  | I64DivS
  | I64DivU
  | I64RemS
  | I64RemU
  | I64And
  | I64Or
  | I64Xor
  | I64Shl
  | I64ShrS
  | I64ShrU
  | I64Rotl
  | I64Rotr
  | F32Abs
  | F32Neg
  | F32Ceil
  | F32Floor
  | F32Trunc
  | F32Nearest
  | F32Sqrt
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Min
  | F32Max
  | F32Copysign
  | F64Abs
  | F64Neg
  | F64Ceil
  | F64Floor
  | F64Trunc
  | F64Nearest
  | F64Sqrt
  | F64Add
  | F64Sub
  | F64Mul
  | F64Div
  | F64Min
  | F64Max
  | F64Copysign
  | I32WrapI64
  | I32TruncF32S
  | I32TruncF32U
  | I32TruncF64S
  | I32TruncF64U
  | I64ExtendI32S
  | I64ExtendI32U
  | I64TruncF32S
  | I64TruncF32U
  | I64TruncF64S
  | I64TruncF64U
  | F32ConvertI32S
  | F32ConvertI32U
  | F32ConvertI64S
  | F32ConvertI64U
  | F32DemoteF64
  | F64ConvertI32S
  | F64ConvertI32U
  | F64ConvertI64S
  | F64ConvertI64U
  | F64PromoteF32
  | I32ReinterpretF32
  | I64ReinterpretF64
  | F32ReinterpretI32
  | F64ReinterpretI64
  deriving (Show, Eq)

data Memory = Memory
  { initial :: Word8,
    max :: Maybe Word8
  }
  deriving (Show, Eq)

data Data = Data
  { memoryIndex :: Word8,
    offset :: [Instruction],
    dataContent :: ByteString
  }
  deriving (Show, Eq)

data Export = Export
  { name :: ByteString,
    exportType :: Word8,
    exportIndex :: Word8
  }
  deriving (Show, Eq)

data Import = Import
  { moduleName :: ByteString,
    importName :: ByteString,
    importType :: Word8
  }
  deriving (Show, Eq)

data SectionType
  = TypeSection
  | FunctionSection
  | CodeSection
  | MemorySection
  | DataSection
  | ExportSection
  | ImportSection
  | UnknownSection Word8
  deriving (Show, Eq)

data Module = Module
  { magic :: ByteString,
    version :: Word32,
    typeSection :: [FuncType],
    functionSection :: [Word32],
    codeSection :: [Function],
    memorySection :: [Memory],
    dataSection :: [Data],
    exportSection :: [Export],
    importSection :: [Import]
  }
  deriving (Show, Eq)
