module Lambda.STLC.CEK

import Iter
import Lambda.Untyped.Scoped.Term
import Data.Fin

%default total
%access public export

mutual
  data Env : Nat -> Type where
    (::) : Clos -> Env x -> Env (S x)  
    Nil : Env Z

  data Clos : Type where
    Cl : Term (S x) -> Env x -> Clos

  data Stack : Type where
    Mt : Stack
    Fun : Clos -> Stack -> Stack
    Arg : Term g -> Env g -> Stack -> Stack
    
  data State = St (Term x) (Env x) Stack
  
  step : State -> Maybe State
  step (St (Var FZ) ((Cl t e) :: _) s) = Just $ St (Lam t) e s
  step (St (Var (FS x)) (_::e) s) = Just $ St (Var x) e s
  step (St (Lam t) e (Arg t1 e1 s)) = Just $ St t1 e1 (Fun (Cl t e) s)
  step (St (Lam t) e (Fun (Cl t1 e1) s)) = Just $ St t1 (Cl t e::e1) s
  step (St (App t u) e s) = Just $ St t e (Arg u e s)  
  step  _ = Nothing   
  
runCEK : Term Z -> (Nat, Maybe State)
runCEK t = iterCount step $ St t Nil Mt
