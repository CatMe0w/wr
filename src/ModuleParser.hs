module ModuleParser (moduleParser) where

import CodeParser (codeSectionParser)
import Data.Attoparsec.ByteString
import DataParser (dataSectionParser)
import ExportParser (exportSectionParser)
import FunctionParser (functionSectionParser)
import ImportParser (importSectionParser)
import MagicParser
import MemoryParser (memorySectionParser)
import SectionParser
import TypeParser
import Wasm

moduleParser :: Parser Module
moduleParser = do
  magic' <- magicParser
  version' <- versionParser
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
          typeSection' <- either fail return $ parseOnly typeSectionParser sectionContent
          parseSections modl {typeSection = typeSection'}
        FunctionSection -> do
          functionSection' <- either fail return $ parseOnly functionSectionParser sectionContent
          parseSections modl {functionSection = map fromIntegral functionSection'}
        CodeSection -> do
          codeSection' <- either fail return $ parseOnly codeSectionParser sectionContent
          parseSections modl {codeSection = codeSection'}
        MemorySection -> do
          memorySection' <- either fail return $ parseOnly memorySectionParser sectionContent
          parseSections modl {memorySection = memorySection'}
        DataSection -> do
          dataSection' <- either fail return $ parseOnly dataSectionParser sectionContent
          parseSections modl {dataSection = dataSection'}
        ExportSection -> do
          exportSection' <- either fail return $ parseOnly exportSectionParser sectionContent
          parseSections modl {exportSection = exportSection'}
        ImportSection -> do
          importSection' <- either fail return $ parseOnly importSectionParser sectionContent
          parseSections modl {importSection = importSection'}
        _ -> parseSections modl
