
{-# OPTIONS -XTypeSynonymInstances -XExistentialQuantification -XTemplateHaskell #-}

module StmtResolver
(
   runStmts,
   StmtEnvironment(..),
   VariableMap,
   variableMapToStringMap
) 
where


import SrcTemplateTree
import VarTypes
import LiftFunction

import qualified Data.Map as Map
import Data.IORef
import Control.Monad
import Data.Char

 
type VariableMap = Map.Map String Variable


data StmtEnvironment = StmtEnvironment
   {
      variables :: VariableMap
   }


type StmtEnvironmentRef = IORef StmtEnvironment


variableMapToStringMap :: VariableMap -> Map.Map String String
variableMapToStringMap = Map.map toString


eval :: StmtEnvironmentRef -> Expr -> IO Variable
eval _ (StrLit str) = return $ StrVar str 

eval _ (IntLit i) = return $ IntVar i

eval _ (BoolLit b) = return $ BoolVar b

eval env (Operator op lExpr rExpr) = do
   left <- eval env lExpr  
   right <- eval env rExpr
   case op of
      "and"     -> return . BoolVar $ castToBool left && castToBool right
      
      "or"      -> return . BoolVar $ castToBool left || castToBool right
      
      "+"       -> case varType left of
         VTString   -> return . StrVar $ castToString left ++ castToString right
         VTInt      -> return . IntVar $ castToInt left + castToInt right  
         otherwise  -> error $ "Operator '" ++ op ++ "' is unknown."
         
      "-"       -> return . IntVar $ castToInt left - castToInt right
      "*"       -> return . IntVar $ castToInt left * castToInt right
      "/"       -> return . IntVar $ castToInt left `div` castToInt right
      "%"       -> return . IntVar $ castToInt left `mod` castToInt right
      
      otherwise -> error $ "Operator '" ++ op ++ "' is unknown." 
      

eval env (UnOperator op expr) = do
   val <- eval env expr
   case op of
      "not"     -> return . BoolVar . not $ castToBool val
      "-"       -> return . IntVar $ - (castToInt val)
      otherwise -> error $ "Operator '" ++ op ++ "' is unknown."
      

eval env (FunctionCall fkt params) =
   case fkt of
      "toLower" -> liftFkt1 env strToLower params 
      "toUpper" -> liftFkt1 env strToUpper params
      "toInt"   -> liftFkt1 env strToInt params
      "first"   -> liftFkt1 env strFirst params
      "last"    -> liftFkt1 env strLast params
      "length"  -> liftFkt1 env strLength params
      "substr"  -> liftFkt3 env subStr params
      otherwise -> error $ "Funktion '" ++ fkt ++ "' is unknown."

      
eval environment (VarExpr name) = do
   env <- readIORef environment
   case Map.lookup name (variables env) of
      Nothing    -> error $ "Variable '" ++ name ++ "' is unknown."
      (Just val) -> return val


eval environment (DefinedExpr name) = do
   env <- readIORef environment
   return $ BoolVar (Map.member name (variables env))


runStmt :: StmtEnvironmentRef -> Stmt -> IO ()
runStmt environment (AssignmentStmt name expression) = do
   val <- eval environment expression
   env <- readIORef environment
   writeIORef environment StmtEnvironment { variables = Map.insert name val (variables env) }


runStmt environment (IfStmt cond thenPart elsePart) = do
   condValue <- eval environment cond
   if castToBool condValue
      then runStmts environment thenPart
      else runStmts environment elsePart
   
   
runStmts :: StmtEnvironmentRef -> [Stmt] -> IO ()
runStmts env stmts = do
   forM_ stmts $ \stmt -> runStmt env stmt
     
     
liftFkt1 :: StmtEnvironmentRef -> (Variable -> Variable) -> [Expr] -> IO Variable
liftFkt1 env fkt exprs = 
   case exprs of
      [expr]    -> liftM fkt $ eval env expr
      otherwise -> error "One funktion parameter expected."

      
liftFkt2 :: StmtEnvironmentRef -> (Variable -> Variable -> Variable) -> [Expr] -> IO Variable
liftFkt2 env fkt exprs = 
   case exprs of
      [expr0, expr1]  -> liftM2 fkt (eval env expr0) (eval env expr1)
      otherwise       -> error "Two funktion parameter expected."      

      
liftFkt3 :: StmtEnvironmentRef -> (Variable -> Variable -> Variable -> Variable) -> [Expr] -> IO Variable
liftFkt3 env fkt exprs = 
   case exprs of
      [expr0, expr1, expr2]  -> liftM3 fkt (eval env expr0) (eval env expr1) (eval env expr2)
      otherwise       -> error "3 funktion parameter expected."      
      

strToLower = $(liftVarFct1 "strToLower'")
   where
      strToLower' :: String -> String
      strToLower' = map toLower    


strToUpper = $(liftVarFct1 "strToUpper'")
   where
      strToUpper' :: String -> String
      strToUpper' = map toUpper

 
strFirst :: Variable -> Variable
strFirst = $(liftVarFct1 "strFirst'")
   where
      strFirst' :: String -> String
      strFirst' = head . (: [])


strLast :: Variable -> Variable
strLast = $(liftVarFct1 "strLast'")
   where
      strLast' :: String -> String
      strLast' = last . (: [])


strToInt :: Variable -> Variable
strToInt var = IntVar . read . castToString $ var


strLength :: Variable -> Variable
strLength var = IntVar . length . castToString $ var
 


subStr :: Variable -> Variable -> Variable -> Variable 
subStr p0 p1 p2 =
   let
      str = castToString p0
      pos = castToInt p1
      len = castToInt p2
   in            
      StrVar $ take len (drop pos str)

