
module SrcGen
(
   generateGroup,
   getAllTemplateGroups
)
where


import SrcTemplateTree
import StmtResolver
import SrcTemplateParser
import Input
import ProgramOptions

import qualified Data.Map as Map
import Data.IORef
import Text.StringTemplate
import Text.Regex.Posix
import System.FilePath
import System.Directory
import Text.ParserCombinators.Parsec.Prim
import Control.Monad

templateGroupFileRegex = ".*stg$"
outFileParamName = "outFile"
outDirParamName = "outDir"
defDir = "."



generateGroup :: FilePath -> IO ()
generateGroup inFilePath = do
   groupTemplate <- parseFromFile srcTemplateGroup inFilePath
   case groupTemplate of
      Left err -> error $ show err
      Right (SrcTemplateGroup _ params srcTemplates) -> do
         homeDir <- getHomeDir
         paramMap <- readParams params homeDir
         forM_ srcTemplates $ saveSrcTemplate paramMap


saveSrcTemplate :: VariableMap -> SrcTemplate -> IO ()
saveSrcTemplate vars (SrcTemplate stmts templateTxt) = do
   envIn <- newIORef $ StmtEnvironment vars
   runStmts envIn stmts
   envOut <- readIORef envIn
   let strVars = variableMapToStringMap $ variables envOut
   let templateWithoutAttr = (newSTMP templateTxt) :: StringTemplate String
   let template = setManyAttrib (Map.toList strVars) templateWithoutAttr
   let outFile = Map.findWithDefault (error "Output file is not specified.") outFileParamName strVars
   let outDir = Map.findWithDefault defDir outDirParamName strVars
   if outFile == "stdout"
      then putStrLn $ toString template
      else writeFile (outDir </> outFile) $ toString template
   
   
getAllTemplateGroups :: FilePath -> IO [(FilePath, SrcTemplateGroupOptions)]
getAllTemplateGroups dir = do
   names <- getDirectoryContents dir
   let tgfpNames = filter (=~ templateGroupFileRegex) names
   forM tgfpNames $ \name -> do
      let inFilePath = dir </> name
      parseResult <- parseFromFile srcTemplateGroupOptions inFilePath 
      case parseResult of          
         Left err -> error $ show err
         Right options -> return (inFilePath, options)
   

   