 -- The Types

 data Val = IntVal Integer
     deriving (Show,Eq)

 data Exp = IntExp Integer
          | IntOpExp String Exp Exp
     deriving (Show,Eq)

 type Env = [(String,Exp)]

 -- Evaluator

 intOps = [ ("+",(+))
          , ("-",(-))
          , ("*",(*))
          , ("/",div)]

 liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
 liftIntOp f _ _ = 0

 eval :: Exp -> Env -> Val
 eval (IntExp i) _ = IntVal i

 eval (IntOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = lookup op intOps
     in liftIntOp f e1 e2
