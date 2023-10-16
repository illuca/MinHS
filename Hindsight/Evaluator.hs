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
           | Nil
           | Cons Integer Value
           -- TODO: others as needed

data Terminal =
           P Value
           -- TODO: others as needed

evalV :: VEnv -> VExp -> Value
evalV e (Num n) = I n
evalV e (Con "True") = B True
evalV e (Con "False") = B False
evalV e (Con "Nil") = Nil

evalV _ _ = error "TODO: implement evalV"
evalC :: VEnv -> CExp -> Terminal


{--
| Add
| Sub
| Mul
| Quot
| Rem
| Neg
| Gt
| Ge
| Lt
| Le
| Eq
| Ne
| Head
| Tail
| Null
--}
evalC env (App (App (Prim op) e1) e2) =
  case op of
    Add -> P(I (v1 + v2))
    Sub -> P(I (v1 - v2))
    Mul -> P(I (v1 * v2))
    Quot -> P(I (v1 `quot` v2))
    Rem -> P(I (v1 `rem` v2))
    Eq -> P(B (v1 == v2))
    Ne -> P(B (v1 /= v2))
    Neg -> P(I (v1 - v2))
    Gt -> P(B (v1 > v2))
    Ge -> P(B (v1 >= v2))
    Lt -> P(B (v1 < v2))
    Le -> P(B (v1 <= v2))
  where
      I v1 = evalV env e1
      I v2 = evalV env e2


evalC e (Produce v1) =
   let v2 = evalV e v1
   in P v2

--evalC e (Produce v1) =
--   P v2
--   where v2 = evalV e v1
--evalC env (Force (Cons x xs)) = vx : vs
--  where
--        I vs = evalV env xs
evalC _ _ = error "TODO: implement evalC"
