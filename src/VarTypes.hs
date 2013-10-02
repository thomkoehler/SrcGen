
module VarTypes
(
   VarType(..),
   Variable(..),
   toString,
   castToString,
   castToBool,
   castToInt,
   varType   
)
where

data VarType = VTString
             | VTBool
             | VTInt
             deriving(Show)



data Variable = StrVar String
              | IntVar Int
              | BoolVar Bool


toString :: Variable -> String
toString (StrVar str) = str
toString (IntVar i) = show i 
toString (BoolVar b) = show b


castToString :: Variable -> String
castToString (StrVar str) = str
castToString _ = error "String expected." 


castToBool :: Variable -> Bool
castToBool (BoolVar b) = b
castToBool _ = error "Bool expected." 


castToInt :: Variable -> Int
castToInt (IntVar i) = i
castToInt _ = error "Int expected."


varType :: Variable -> VarType
varType (StrVar _) = VTString
varType (IntVar _) = VTInt 
varType (BoolVar _) = VTBool

