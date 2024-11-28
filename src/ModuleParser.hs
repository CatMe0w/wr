module ModuleParser (parseModule) where

import CodeParser (parseCodeSection)
import Data.Attoparsec.ByteString
import DataParser
import ExportParser
import FunctionParser
import ImportParser
import MagicParser
import MemoryParser
import SectionParser
import TypeParser
import Wasm

parseModule :: Parser Module
parseModule = do
  magic' <- parseMagic
  version' <- parseVersion
  let emptyModule = Module magic' version' [] [] [] [] [] [] []
  parseSections emptyModule

parseSections :: Module -> Parser Module
parseSections modl = do
  atEnd' <- atEnd
  if atEnd'
    then return modl
    else do
      (sectionType, sectionContent) <- parseSection
      case sectionType of
        TypeSection -> do
          typeSection' <- either fail return $ parseOnly parseTypeSection sectionContent
          parseSections modl {typeSection = typeSection'}
        FunctionSection -> do
          functionSection' <- either fail return $ parseOnly parseFunctionSection sectionContent
          parseSections modl {functionSection = functionSection'}
        CodeSection -> do
          codeSection' <- either fail return $ parseOnly parseCodeSection sectionContent
          parseSections modl {codeSection = codeSection'}
        MemorySection -> do
          memorySection' <- either fail return $ parseOnly parseMemorySection sectionContent
          parseSections modl {memorySection = memorySection'}
        DataSection -> do
          dataSection' <- either fail return $ parseOnly parseDataSection sectionContent
          parseSections modl {dataSection = dataSection'}
        ExportSection -> do
          exportSection' <- either fail return $ parseOnly parseExportSection sectionContent
          parseSections modl {exportSection = exportSection'}
        ImportSection -> do
          importSection' <- either fail return $ parseOnly parseImportSection sectionContent
          parseSections modl {importSection = importSection'}
        _ -> parseSections modl
