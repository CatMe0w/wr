module Wasm
  ( ValueType (..),
    FuncType (..),
    Function (..),
    Instruction (..),
    SectionCode (..),
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
  | End
  deriving (Show, Eq)

data SectionCode
  = TypeSection
  | FunctionSection
  | CodeSection
  | UnknownSection Word8
  deriving (Show, Eq)

data Module = Module
  { magic :: ByteString,
    version :: Word32,
    typeSection :: [FuncType],
    functionSection :: [Word32],
    codeSection :: [Function]
  }
  deriving (Show, Eq)