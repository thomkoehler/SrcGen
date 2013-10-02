
module SrcTemplateParser
(
   srcTemplateGroup,
   srcTemplateGroupOptions
)
where



import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import SrcTemplateTree
import SrcTemplateLexer



srcTemplateGroup :: Parser SrcTemplateGroup
srcTemplateGroup = do
   whiteSpace
   opts <- srcTemplateGroupOptions      
   ps <- option [] params
   templates <- many1 srcTemplate
   return $ SrcTemplateGroup opts ps templates        


srcTemplateGroupOptions :: Parser SrcTemplateGroupOptions
srcTemplateGroupOptions = do
   whiteSpace
   reserved "options"
   symbol "{"
   desc <- optionDescription 
   symbol "}"
   return desc
   

optionDescription :: Parser SrcTemplateGroupOptions
optionDescription = do
   reserved "description"
   symbol "="
   opt <- stringLit
   return SrcTemplateGroupOptions    
       {
         stgoDescription = Just opt
       }  


srcTemplate :: Parser SrcTemplate
srcTemplate = do
   sts <- option [] stmts
   st <- strTemplate
   return $ SrcTemplate sts st


params :: Parser [Param]
params = do
   reserved "params"
   symbol "{"
   params <- param `sepBy` (symbol ",") 
   symbol "}"
   return params


param :: Parser Param
param = do
   idr <- identifier
   description <- option idr $ squares stringLit 
   paramConfig <- option SimpleParam $ (symbol "=" >> choice [defaultValue, valueHistory]) 
   return $ StrParam idr description paramConfig

  
valueHistory :: Parser (ParamConfig String)
valueHistory = do
   reserved "history"
   histName <- parens identifier
   return $ ParamWithHistroy histName
   
   
defaultValue :: Parser (ParamConfig String)
defaultValue = do
   defValue <- stringLit
   return $ ParamWithDefaultValue defValue
          

strTemplate :: Parser String
strTemplate = do
   reserved "$$"
   str <- manyTill anyChar (reserved "$$") 
   return str


stmts :: Parser [Stmt]
stmts = do
   symbol "{" 
   res <- many $ do
      st <- choice [ifStmt, assignmentStmt]
      return st
   symbol "}"
   return res
   

assignmentStmt :: Parser Stmt   
assignmentStmt = do
   idr <- identifier
   symbol "="
   ex <- expr
   symbol ";"
   return $ AssignmentStmt idr ex    


ifStmt :: Parser Stmt
ifStmt = do
   reserved "if"
   cond <- parens expr
   thenPart <- stmts
   elsePart <- option [] $ do
      reserved "else"
      stmts
   return $ IfStmt cond thenPart elsePart            
   
         
expr :: Parser Expr                 
expr = buildExpressionParser operators simpleExpr <?> "Expression"      
     

operators =
   [[ prefix "-"],
    [ op "*" AssocLeft, op "/" AssocLeft ],
    [ op "+" AssocLeft, op "-" AssocLeft ],
    [ prefix "not"],
    [op "and" AssocNone],
    [op "or" AssocNone]]
   where
      op name = Infix $ do
         reservedOp name
         return (\param0 param1 -> Operator name param0 param1)
         
      prefix name = Prefix $ do
         reservedOp name
         return (\param -> UnOperator name param)         
         
         
simpleExpr :: Parser Expr         
simpleExpr =
   choice [trueExpr, falseExpr, definedExpr, strExpr, intExpr, variable, parens expr, funktionCall]          


intExpr :: Parser Expr
intExpr = do 
   i <- intLit
   return $ IntLit (fromIntegral i)


strExpr :: Parser Expr
strExpr = do 
   str <- stringLit
   return $ StrLit str
   
   
trueExpr :: Parser Expr
trueExpr = do
   reserved "true"
   return $ BoolLit True     
   
   
falseExpr :: Parser Expr
falseExpr = do
   reserved "false"
   return $ BoolLit False   
   
   
definedExpr :: Parser Expr
definedExpr = do
   reserved "def"
   idr <- identifier
   return $ DefinedExpr idr 
   
   
variable :: Parser Expr
variable = do
   char '$'
   idr <- identifier
   return $ VarExpr idr      


funktionCall :: Parser Expr
funktionCall = do
   idr <- identifier
   params <- (parens (expr `sepBy` (symbol ",")))
   return $ FunctionCall idr params
