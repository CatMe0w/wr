module Wasm
  ( ValueType (..),
    FuncType (..),
    Function (..),
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
import Data.Word (Word32)

data ValueType = I32 | I64 deriving (Show, Eq)

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

-- todo: add more instructions
data Instruction
  = LocalGet Word8
  | LocalSet Word8
  | I32Const Word32
  | End
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
