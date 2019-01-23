module Lambda.Untyped.Scoped.KAM

import Iter
import Lambda.Untyped.Scoped.Term
import Data.Fin

%default total
%access public export

mutual 
  data Env : Nat -> Type where
    (::) : Clos -> Env x -> Env (S x)  
    Nil : Env Z

  data Clos = Cl (Term x) (Env x)

  Stack : Type
  Stack = List Clos

  data State = St (Term x) (Env x) Stack
  
 step : State -> Maybe State
 step (St (Var FZ) ((Cl t e) :: _) s) = Just $ St t e s
 step (St (Var (FS x)) (_::e) s) = Just $ St (Var x) e s
 step (St (Lam t) e (c :: s)) = Just $ St t (c::e) s
 step (St (App t u) e s) = Just $ St t e ((Cl u e) :: s) 
 step _ = Nothing

runKAM : Term Z -> (Nat, Maybe State)
runKAM t = iterCount step $ St t Nil []

private
test0 : runKAM Term0 = (7, Just $ St (Lam V0) Nil [])
test0 = Refl

private
test1 : runKAM Term1 = (6, Just $ St (Lam V0) Nil [])
test1 = Refl

private
test2 : runKAM Term2 = (6, Just $ St (Lam V0) Nil [])
test2 = Refl
