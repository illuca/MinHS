{-# LANGUAGE BangPatterns #-}

module Hindsight.Optimiser where

import MinHS.Syntax(Op(..),TyCon(..))
import Hindsight.Syntax


optimiser :: CBind -> CBind
optimiser (CBind x ty args e) = CBind x ty args (optimiseC e)

substituteV :: VExp -> Id -> VExp -> VExp
substituteV e x v =
  case e of
    Var y -> v
    Thunk t ->
      let t' = optimiseC t
      in Thunk t'
    _ -> e


substituteC :: CExp -> Id -> VExp -> CExp
substituteC (Produce e) x v = Produce (substituteV e x v)
substituteC (Force e) x v = Force (substituteV e x v)
{-
    if x' == x, then do optimze to current reduce
    else do do optimze to current reduce, get res, then do S to res
-}
substituteC (Reduce x' c1 c2) x v
  | x == x' = optimiseC (Reduce x' c1 c2)
  | otherwise =
    let cexp' = optimiseC (Reduce x' c1 c2)
    in substituteC cexp' x v

--do S to cexp and vexp
substituteC (App cexp vexp) x v =
   let cexp' = substituteC cexp x v
       vexp' = substituteV vexp x v
   in App cexp' vexp'
{-
  if x is not in ids, then do S to e
  else do nothing

  for cexp in function, we do optimize
-}
substituteC (Recfun (CBind a b ids e)) x v =
  let
      e' = optimiseC e
      -- get e' then discuss case
      e''
        | x `elem` ids = e'
        | otherwise = substituteC e' x v
      bind' = CBind a b ids e''
  in Recfun bind'

substituteC (Prim op) x v = Prim op
{-
  do S to v,c1,c2
-}
substituteC (If vexp c1 c2) x v =
  let vexp' = substituteV vexp x v
      c1' = substituteC c1 x v
      c2' = substituteC c2 x v
  in If vexp' c1' c2'
substituteC c _ _ = c

optimiseC :: CExp -> CExp
optimiseC (Force (Thunk t)) = optimiseC t
optimiseC (Reduce id (Produce vexp) cexp) = substituteC cexp id vexp

optimiseC (App (Recfun (CBind f _ [x] c)) v) = substituteC c x v
optimiseC (App (Recfun (CBind f _ [] c)) v) = optimiseC c

optimiseC (App cexp v) =
  let cexp' = optimiseC cexp
  in optimiseC (App cexp' v)
optimiseC c = c