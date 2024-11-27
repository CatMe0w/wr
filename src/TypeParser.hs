{-# LANGUAGE LambdaCase #-}

module TypeParser (valueTypeParser, funcTypeParser, typeSectionParser) where

import Data.Attoparsec.ByteString
import Wasm hiding (params, results)

valueTypeParser :: Parser ValueType
valueTypeParser = anyWord8 >>= \case
  0x7F -> pure I32
  0x7E -> pure I64
  _ -> fail "Unknown value type"

funcTypeParser :: Parser FuncType
funcTypeParser = do
  _ <- word8 0x60
  numParams <- anyWord8
  params <- count (fromIntegral numParams) valueTypeParser
  numResults <- anyWord8
  results <- count (fromIntegral numResults) valueTypeParser
  return $ FuncType params results

typeSectionParser :: Parser [FuncType]
typeSectionParser = anyWord8 >>= flip count funcTypeParser . fromIntegral
