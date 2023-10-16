module Hindsight.Optimiser where

import MinHS.Syntax(Op(..),TyCon(..))
import Hindsight.Syntax

optimiser :: CBind -> CBind
optimiser (CBind x ty args e) = CBind x ty args (optimiseC e)

optimiseC :: CExp -> CExp
optimiseC c = error "TODO: implement optimiseC"
