{-# LANGUAGE BangPatterns #-}

module Hindsight.Evaluator where

import Data.List
import qualified MinHS.Env as E
import Hindsight.Syntax
import MinHS.Syntax(Op(..),TyCon(..))
import MinHS.Pretty(datacon,numeric,ANSIPretty(..))
import Prettyprinter as PP
import Prettyprinter.Render.Terminal

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
           | Tk VEnv VExp
           | VF VEnv CExp
           deriving (Show)
           -- TODO: others as needed

data Terminal = P Value
              | TF VEnv CExp
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

evalV env (Thunk t) = Tk env (Thunk t)

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

evalC env (Produce vexp) =
   let v = evalV env vexp
   in P v

evalC env (App (App (Force (Con "Cons")) vexp1) vexp2) =
  let I v1 = evalV env vexp1
      v2 = evalV env vexp2
  in P (Cons v1 v2)

evalC env (Recfun cbind) =
  TF env (Recfun cbind)

evalC env (Force vexp) =
  let v = evalV env vexp
  in case v of
    Tk env' (Thunk t) -> evalC env' t
    VF env' recfun -> evalC env' recfun
    _ -> error $ "Arguments after Force is not valid." ++ show v

evalC env (App cexp vexp) =
  {-
    cexp can be recfun, force, prim operators
    if recfun, then eval it to TF
  -}
  let t = evalC env cexp
      v = evalV env vexp
  in case t of
--    CBind Id CType [Id] CExp
    TF env' (Recfun cbind) ->
          let CBind f ty names body = cbind
              name = head names
              newEnv = E.addAll env' [(f, toV t),(name, v)]
          in evalC newEnv body
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
toV (TF env recfun) = VF env recfun