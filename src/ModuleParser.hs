module ModuleParser (moduleParser) where

import CodeParser (codeSectionParser)
import Data.Attoparsec.ByteString
import FunctionParser (functionSectionParser)
import MagicParser
import SectionParser
import TypeParser
import Wasm

moduleParser :: Parser Module
moduleParser = do
  magic' <- magicParser
  version' <- versionParser
  let emptyModule = Module magic' version' [] [] []
  parseSections emptyModule

parseSections :: Module -> Parser Module
parseSections modl = do
  atEnd' <- atEnd
  if atEnd'
    then return modl
    else do
      (sectionCode, sectionContent) <- parseSection
      case sectionCode of
        TypeSection -> do
          typeSection' <- either fail return $ parseOnly typeSectionParser sectionContent
          parseSections modl {typeSection = typeSection'}
        FunctionSection -> do
          functionSection' <- either fail return $ parseOnly functionSectionParser sectionContent
          parseSections modl {functionSection = map fromIntegral functionSection'}
        CodeSection -> do
          codeSection' <- either fail return $ parseOnly codeSectionParser sectionContent
          parseSections modl {codeSection = codeSection'}
        _ -> parseSections modl
