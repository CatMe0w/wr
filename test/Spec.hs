module Main (main) where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import ModuleParser (moduleParser)
import Test.Hspec
import Wasm

-- main :: IO ()
-- main = do
--   wasm <- BS.readFile "test.wasm"
--   let result = parseOnly moduleParser wasm
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
                codeSection = []
              }
      let result = parseOnly moduleParser wasm
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
                  ]
              }
      let result = parseOnly moduleParser wasm
      case result of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right modl -> modl `shouldBe` expected
