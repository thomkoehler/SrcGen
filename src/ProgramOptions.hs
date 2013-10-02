
module ProgramOptions
(
   getHomeDir,
   checkHomeDir
)
where


import System.Environment 
import System.Directory
import System.FilePath



getHomeDir :: IO FilePath
getHomeDir = do
   userHomeDir <- getHomeDirectory
   return $ userHomeDir </> ".SrcGen" 
         

checkHomeDir :: IO ()
checkHomeDir = do
   homeDir <- getHomeDir
   createDirectoryIfMissing True homeDir

    
   

   