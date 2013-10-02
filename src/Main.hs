
{-# OPTIONS -XTemplateHaskell #-}

module Main where

import SrcGen
import ProgramOptions
import Input


import System.Environment

import qualified Control.Exception as E
     

run homeDir = do
   templateGroups <- getAllTemplateGroups homeDir
   if null templateGroups
      then error $ "No template source files found in directory '" ++ homeDir ++ "'."
      else do
         templateGroupFileName <- selectTemplateGroup templateGroups
         if null templateGroupFileName
            then return ()
            else generateGroup templateGroupFileName


main = E.catch
   (
      do
         args <- getArgs
         case args of
            [homeDir] -> run homeDir
            
            otherwise -> do 
               checkHomeDir
               homeDir <- getHomeDir
               run homeDir
   )
   (
      \err -> do
         putStrLn $ show (err :: E.SomeException)
         getChar
         return ()
   ) 
   
