
module SrcTemplateTree where


data ParamConfig valueType = SimpleParam 
                           | ParamWithDefaultValue valueType
                           | ParamWithHistroy String
                           deriving(Show)  



data Param = StrParam String String (ParamConfig String)  
           deriving Show


data SrcTemplateGroupOptions = SrcTemplateGroupOptions
   {
      stgoDescription :: Maybe String
   }
   deriving Show


data SrcTemplateGroup = SrcTemplateGroup SrcTemplateGroupOptions [Param] [SrcTemplate]   
                      deriving Show


data SrcTemplate = SrcTemplate [Stmt] String
                 deriving Show 


data Expr = StrLit String 
          | BoolLit Bool
          | IntLit Int
          | Operator String Expr Expr
          | UnOperator String Expr
          | FunctionCall String [Expr]
          | VarExpr String
          | DefinedExpr String
          deriving Show
          
          
data Stmt = AssignmentStmt String Expr
          | IfStmt Expr [Stmt] [Stmt]    
          deriving Show      
                 