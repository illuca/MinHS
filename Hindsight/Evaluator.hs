{-# LANGUAGE BangPatterns #-}

module Hindsight.Evaluator where

import Data.List
import qualified MinHS.Env as E
import Hindsight.Syntax
import MinHS.Syntax(Op(..),TyCon(..))
import MinHS.Pretty(datacon,numeric,ANSIPretty(..))
import Prettyprinter as PP
import Prettyprinter.Render.Terminal
import qualified Data.Map as M

instance {-# OVERLAPPING #-} ANSIPretty Value where
  ansipp (I i) = numeric $ i
  ansipp (B b) = datacon $ show b
  ansipp (Nil) = datacon "Nil"
  ansipp (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> ansipp v)
  ansipp _ = undefined -- should not ever be used

instance Pretty Value where
  pretty = unAnnotate . ansipp

type VEnv = E.Env Value

evaluate :: CBind -> Value
evaluate (CBind _ _ _ e) =
  case evalC E.empty e of
    P v -> v
    _   -> error "Top-level computation produced non-ground value"

--- From here is where you should put your work!
data Value = I Integer
           | B Bool
           | Nil | Cons Integer Value
           | VThunk VEnv VExp
           | VFunc VEnv CExp -- value func
           deriving (Show)
           -- TODO: others as needed

data Terminal = P Value
              | TFunc VEnv CExp -- terminal func
           -- TODO: others as needed

evalV :: VEnv -> VExp -> Value
evalV env (Num n) = I n
evalV env (Con "True") = B True
evalV env (Con "False") = B False
evalV env (Con "Nil") = Nil

evalV env (Var x) =
  case E.lookup env x of
    Just v  -> v
    Nothing -> error $ "Variable " ++ x ++ " not found in environment"

evalV env (Thunk t) = VThunk env (Thunk t)

evalV _ _ = error "TODO: implement evalV"

evalC :: VEnv -> CExp -> Terminal

evalC env (If vexp1 cexp1 cexp2) =
  let B v1 = evalV env vexp1
  in case v1 of
     True -> evalC env cexp1
     _ -> evalC env cexp2

evalC env (App (App (Prim op) vexp1) vexp2) =
  case op of
    Add -> P(I (a + b))
    Sub -> P(I (a - b))
    Mul -> P(I (a * b))
    Quot -> if b == 0
            then error "The denominator cannot be 0."
            else P(I (a `quot` b))
    Rem -> P(I (a `rem` b))
    Eq -> P(B (a == b))
    Ne -> P(B (a /= b))
    Neg -> P(I (a - b))
    Gt -> P(B (a > b))
    Ge -> P(B (a >= b))
    Lt -> P(B (a < b))
    Le -> P(B (a <= b))
  where
    v1 = evalV env vexp1
    v2 = evalV env vexp2
    a = case v1 of
      I i -> i
      _ -> error "Unexpected value"
    b = case v2 of
      I i -> i
      _ -> error "Unexpected value"

evalC env (App (Prim Neg) vexp) =
  let I v = evalV env vexp
  in P (I (- v))


evalC env (App (Prim Null) vexp) =
  let v = evalV env vexp
  in case v of
     Nil -> P (B True)
     Cons _ _ -> P (B False)

evalC env (App (Prim Tail) vexp) =
  let res = evalV env vexp
  in case res of
    Cons v vs -> P vs
    _ -> error "List cannot be empty."

evalC env (App (Prim Head) vexp) =
  let res = evalV env vexp
  in case res of
    Cons v vs -> P (I v)
    _ -> error "List cannot be empty."

-- support 0 argument for + - * /
evalC env (Prim op) = P (evalV env (Thunk (Prim op)))
evalC env (Force (Con "Cons")) = P (evalV env (Thunk (Force (Con "Cons"))))

---- support 1 argument for + - * / Cons begin ----
-- support operations like 10-, 10+, 8*
evalC env (App (Prim op) vexp) = P (evalV env (Thunk (App (Prim op) vexp)))
-- support operations like Cons 1
evalC env (App (Force (Con "Cons")) vexp) = P (evalV env (Thunk (App (Force (Con "Cons")) vexp)))
---- support 1 argument for + - * / Cons end ----

evalC env (Produce vexp) =
   let v = evalV env vexp
   in P v


evalC env (App (App (Force (Con "Cons")) vexp1) vexp2) =
  let I v1 = evalV env vexp1
      v2 = evalV env vexp2
  in P (Cons v1 v2)

evalC env (Recfun cbind) =
  TFunc env (Recfun cbind)

evalC env (Force vexp) =
  let v = evalV env vexp
  in case v of
    VThunk env' (Thunk t) -> evalC env' t
    VFunc env' recfun -> evalC env' recfun
    _ -> error $ "Arguments after Force is not valid." ++ show v

evalC env (App cexp vexp) =
  {-
    cexp can be recfun, force, prim operators
    if recfun, then eval it to TF
  -}
  let t = evalC env cexp
      v = evalV env vexp
  -- in error $ "Some error message: " ++ show env
  in case t of
    -- CBind Id CType [Id] CExp
    TFunc env' (Recfun cbind) ->
          let CBind f ty names body = cbind
              name = head names
              newEnv = E.addAll env' [(f, toV t),(name, v)]
          in evalC newEnv body
    P (VThunk env' (Thunk app)) ->
        let newEnv = expandEnv env env'
        in evalC newEnv (App app vexp)
    _ -> error "Invalid argument."


evalC env (Reduce id cexp1 cexp2) =
  let res = evalC env cexp1
      -- strict evaluation, bind result of c-exp to name
      !_ = res `seq` ()
      P v1 = res
      newEnv =  E.add env (id, v1)
  in evalC newEnv cexp2

evalC env (Let vbinds cexp) =
  let newBindings = [(id, evalV env vexp) | VBind id _ vexp <- vbinds]
      newEnv = E.addAll env newBindings
  in evalC newEnv cexp

toV :: Terminal -> Value
toV (TFunc env recfun) = VFunc env recfun

expandEnv :: VEnv -> VEnv -> VEnv
expandEnv (E.Env env) (E.Env env') = E.Env (M.union env env')