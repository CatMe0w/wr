module Main (main) where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import ModuleParser (parseModule)
import Test.Hspec
import Wasm
import Prelude hiding (max)

-- main :: IO ()
-- main = do
--   wasm <- BS.readFile "test.wasm"
--   let result = parseOnly parseModule wasm
--   case result of
--     Left err -> putStrLn $ "Parse error: " ++ err
--     Right modl -> print modl

main :: IO ()
main = hspec $ do
  describe "ModuleParser" $ do
    it "should correctly parse a wasm module with only the type section" $ do
      let wasm =
            BS.pack
              [ 0x00,
                0x61,
                0x73,
                0x6D, -- WASM_BINARY_MAGIC
                0x01,
                0x00,
                0x00,
                0x00, -- WASM_BINARY_VERSION
                0x01, -- section code
                0x0D, -- section size
                0x02, -- num types
                0x60,
                0x02,
                0x7F,
                0x7E,
                0x00, -- type 0
                0x60,
                0x02,
                0x7E,
                0x7F,
                0x02,
                0x7F,
                0x7E -- type 1
              ]
      let expected =
            Module
              { magic = BS.pack [0x00, 0x61, 0x73, 0x6D],
                version = 1,
                typeSection = [FuncType [I32, I64] [], FuncType [I64, I32] [I32, I64]],
                functionSection = [],
                codeSection = [],
                memorySection = [],
                dataSection = [],
                exportSection = [],
                importSection = []
              }
      let result = parseOnly parseModule wasm
      case result of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right modl -> modl `shouldBe` expected

    it "should correctly parse a wasm module with the type, function, and code sections" $ do
      let wasm =
            BS.pack
              [ 0x00,
                0x61,
                0x73,
                0x6D,
                0x01,
                0x00,
                0x00,
                0x00,
                0x01,
                0x0D,
                0x02,
                0x60,
                0x02,
                0x7F,
                0x7E,
                0x00,
                0x60,
                0x02,
                0x7E,
                0x7F,
                0x02,
                0x7F,
                0x7E,
                0x03,
                0x04,
                0x03,
                0x00,
                0x01,
                0x00,
                0x0A,
                0x0E,
                0x03,
                0x02,
                0x00,
                0x0B,
                0x06,
                0x00,
                0x20,
                0x01,
                0x20,
                0x00,
                0x0B,
                0x02,
                0x00,
                0x0B
              ]
      let expected =
            Module
              { magic = BS.pack [0x00, 0x61, 0x73, 0x6D],
                version = 1,
                typeSection =
                  [ FuncType {params = [I32, I64], results = []},
                    FuncType {params = [I64, I32], results = [I32, I64]}
                  ],
                functionSection = [0, 1, 0],
                codeSection =
                  [ Function {locals = [], code = [End]},
                    Function {locals = [], code = [LocalGet 1, LocalGet 0, End]},
                    Function {locals = [], code = [End]}
                  ],
                memorySection = [],
                dataSection = [],
                exportSection = [],
                importSection = []
              }
      let result = parseOnly parseModule wasm
      case result of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right modl -> modl `shouldBe` expected

    it "should correctly parse a wasm module with all 7 sections" $ do
      let wasm =
            BS.pack
              [ 0x00,
                0x61,
                0x73,
                0x6d,
                0x01,
                0x00,
                0x00,
                0x00,
                0x01,
                0x12,
                0x03,
                0x60,
                0x02,
                0x7f,
                0x7e,
                0x00,
                0x60,
                0x02,
                0x7e,
                0x7f,
                0x02,
                0x7f,
                0x7e,
                0x60,
                0x02,
                0x7e,
                0x7f,
                0x00,
                0x02,
                0x14,
                0x01,
                0x03,
                0x65,
                0x6e,
                0x76,
                0x0c,
                0x69,
                0x6d,
                0x70,
                0x6f,
                0x72,
                0x74,
                0x65,
                0x64,
                0x46,
                0x75,
                0x6e,
                0x63,
                0x00,
                0x00,
                0x03,
                0x04,
                0x03,
                0x00,
                0x01,
                0x02,
                0x05,
                0x03,
                0x01,
                0x00,
                0x01,
                0x07,
                0x22,
                0x04,
                0x06,
                0x6d,
                0x65,
                0x6d,
                0x6f,
                0x72,
                0x79,
                0x02,
                0x00,
                0x05,
                0x66,
                0x75,
                0x6e,
                0x63,
                0x41,
                0x00,
                0x01,
                0x05,
                0x66,
                0x75,
                0x6e,
                0x63,
                0x42,
                0x00,
                0x02,
                0x05,
                0x66,
                0x75,
                0x6e,
                0x63,
                0x43,
                0x00,
                0x03,
                0x0a,
                0x0e,
                0x03,
                0x02,
                0x00,
                0x0b,
                0x06,
                0x00,
                0x20,
                0x01,
                0x20,
                0x00,
                0x0b,
                0x02,
                0x00,
                0x0b,
                0x0b,
                0x2b,
                0x01,
                0x00,
                0x41,
                0x00,
                0x0b,
                0x25,
                0x44,
                0x6f,
                0x20,
                0x6e,
                0x6f,
                0x74,
                0x20,
                0x67,
                0x6f,
                0x20,
                0x67,
                0x65,
                0x6e,
                0x74,
                0x6c,
                0x65,
                0x20,
                0x69,
                0x6e,
                0x74,
                0x6f,
                0x20,
                0x74,
                0x68,
                0x61,
                0x74,
                0x20,
                0x67,
                0x6f,
                0x6f,
                0x64,
                0x20,
                0x6e,
                0x69,
                0x67,
                0x68,
                0x74
              ]
      let expected =
            Module
              { magic = BS.pack [0x00, 0x61, 0x73, 0x6D],
                version = 1,
                typeSection =
                  [ FuncType {params = [I32, I64], results = []},
                    FuncType {params = [I64, I32], results = [I32, I64]},
                    FuncType {params = [I64, I32], results = []}
                  ],
                functionSection = [0, 1, 2],
                codeSection =
                  [ Function {locals = [], code = [End]},
                    Function {locals = [], code = [LocalGet 1, LocalGet 0, End]},
                    Function {locals = [], code = [End]}
                  ],
                memorySection = [Memory {initial = 0, max = Nothing}],
                dataSection =
                  [ Data
                      { memoryIndex = 0,
                        offset = [I32Const 0, End],
                        dataContent = BS.pack (map (fromIntegral . fromEnum) "Do not go gentle into that good night")
                      }
                  ],
                exportSection =
                  [ Export {name = BS.pack (map (fromIntegral . fromEnum) "memory"), exportType = 2, exportIndex = 0},
                    Export {name = BS.pack (map (fromIntegral . fromEnum) "funcA"), exportType = 0, exportIndex = 1},
                    Export {name = BS.pack (map (fromIntegral . fromEnum) "funcB"), exportType = 0, exportIndex = 2},
                    Export {name = BS.pack (map (fromIntegral . fromEnum) "funcC"), exportType = 0, exportIndex = 3}
                  ],
                importSection =
                  [ Import
                      { moduleName = BS.pack (map (fromIntegral . fromEnum) "env"),
                        importName = BS.pack (map (fromIntegral . fromEnum) "importedFunc"),
                        importType = 0
                      }
                  ]
              }
      let result = parseOnly parseModule wasm
      case result of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right modl -> modl `shouldBe` expected
