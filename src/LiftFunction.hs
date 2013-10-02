
{-# OPTIONS -XTemplateHaskell #-}

module LiftFunction
(
   liftVarFct1
) 
where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax



typeList :: String -> Q [Type]
typeList name = do
   info <- qReify $ mkName name
   case info of
      VarI _ t _ _ -> return $ typeList t
      
   where
      typeList :: Type -> [Type]
      typeList (ConT t) = [ConT t]
      typeList (AppT (AppT _ (ConT t)) rest) = (ConT t) : typeList rest
      typeList t = error $ "Only simple types allowed: " ++ show t      


typeName (ConT name) = nameBase name

typeToVarFct t = case typeName t of
   "String" -> ConE $ mkName "StrVar"         
   "Int" -> ConE $ mkName "IntVar"
   "Bool" -> ConE $ mkName "BoolVar"


typeToCastFct t = case typeName t of
   "String" -> VarE $ mkName "castToString"         
   "Int" -> VarE $ mkName "castToInt"
   "Bool" -> VarE $ mkName "castToBool"        


liftVarFct1 :: String -> Q Exp
liftVarFct1 name =  do
   [paramType, resultType] <- typeList name
   let 
      fct = VarE $ mkName name
      castFct = typeToCastFct paramType 
      varFct = typeToVarFct resultType
   x <- newName "x"    
   return $ LamE [VarP x] (AppE varFct (AppE fct (AppE castFct (VarE x)))) 
     
