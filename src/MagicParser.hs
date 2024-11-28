{-# LANGUAGE OverloadedStrings #-}

module MagicParser (parseMagic, parseVersion) where

import qualified Data.Attoparsec.Binary as Bin
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word (Word32)

parseMagic :: Parser ByteString
parseMagic = string "\0asm"

parseVersion :: Parser Word32
parseVersion = Bin.anyWord32le
