
module Input
(
   readParams,
   selectTemplateGroup
)
where


import SrcTemplateTree
import StmtResolver
import VarTypes

import Data.Maybe
import System.IO
import System.Directory
import System.FilePath
import Text.Printf
import Control.Monad
import qualified Data.Map as Map
import System.Console.Haskeline


readParams :: [Param] -> FilePath -> IO VariableMap
readParams params homeDir = foldM readParam Map.empty params
   where
      readParam :: VariableMap -> Param -> IO VariableMap
      readParam paramMap (StrParam ident description paramConfig) = 
         case paramConfig of
            SimpleParam -> do
               let settings = Settings { complete = noCompletion, historyFile = Nothing, autoAddHistory = False }
               line <- runInputT settings $ getInputLine (description ++ " : ")
               case line of
                  Nothing   -> return paramMap
                  Just ""   -> return paramMap
                  Just lstr -> return $ Map.insert ident (StrVar lstr) paramMap
                  
            ParamWithDefaultValue value -> do                  
               let settings = Settings { complete = noCompletion, historyFile = Nothing, autoAddHistory = False }
               line <- runInputT settings $ getInputLine (description ++ "[" ++ value ++ "] : ")
               case line of
                  Nothing   -> return $ Map.insert ident (StrVar value) paramMap
                  Just ""   -> return $ Map.insert ident (StrVar value) paramMap
                  Just lstr -> return $ Map.insert ident (StrVar lstr) paramMap
                 
            ParamWithHistroy historyName -> do
               let historyFileName = homeDir </> historyName ++ ".history"
               let settings = Settings { complete = noCompletion, historyFile = Just historyFileName, autoAddHistory = True }
               line <- runInputT settings $ getInputLine (description ++ " : ")
               case line of
                  Nothing   -> return paramMap
                  Just ""   -> return paramMap
                  Just lstr -> return $ Map.insert ident (StrVar lstr) paramMap
                  
                  
selectTemplateGroup :: [(FilePath, SrcTemplateGroupOptions)] -> IO FilePath
selectTemplateGroup templates = do
   putStrLn "\nString Templates"
   putStrLn "================="
   printTemplateGroups templates 0
   putStr "\n> "
   hFlush stdout
   line <- getLine
   putStrLn ""
   if null line
      then return ""
      else do  
         let (file, _) = templates !! (read line)
         return file
   where
      printTemplateGroups :: [(FilePath, SrcTemplateGroupOptions)] -> Int -> IO ()
      printTemplateGroups [] _ = return ()
      printTemplateGroups ((_, options):xs) idx = do
         printf "%5i : %s\n" idx (fromJust (stgoDescription options))
         printTemplateGroups xs $ idx + 1
           
           
