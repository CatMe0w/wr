{-# LANGUAGE OverloadedStrings #-}

module MagicParser (magicParser, versionParser) where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Word (Word32)
import qualified Data.Attoparsec.Binary as Bin

magicParser :: Parser ByteString
magicParser = string "\0asm"

versionParser :: Parser Word32
versionParser = Bin.anyWord32le
