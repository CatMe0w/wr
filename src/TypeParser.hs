{-# LANGUAGE LambdaCase #-}

module TypeParser (parseValueType, parseFuncType, parseTypeSection) where

import Data.Attoparsec.ByteString
import Wasm hiding (params, results)

parseValueType :: Parser ValueType
parseValueType =
  anyWord8 >>= \case
    0x7F -> pure I32
    0x7E -> pure I64
    0x7D -> pure F32
    0x7C -> pure F64
    _ -> fail "Unknown value type"

parseFuncType :: Parser FuncType
parseFuncType = do
  _ <- word8 0x60
  numParams <- anyWord8
  params <- count (fromIntegral numParams) parseValueType
  numResults <- anyWord8
  results <- count (fromIntegral numResults) parseValueType
  return $ FuncType params results

parseTypeSection :: Parser [FuncType]
parseTypeSection = anyWord8 >>= flip count parseFuncType . fromIntegral
