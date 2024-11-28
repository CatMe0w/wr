{-# LANGUAGE LambdaCase #-}

module TypeParser (parseValueType, parseTypeSection) where

import Data.Attoparsec.ByteString
import LEB128Parser
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
  numParams <- parseU32
  params <- count (fromIntegral numParams) parseValueType
  numResults <- parseU32
  results <- count (fromIntegral numResults) parseValueType
  return $ FuncType params results

parseTypeSection :: Parser [FuncType]
parseTypeSection = parseU32 >>= flip count parseFuncType . fromIntegral
