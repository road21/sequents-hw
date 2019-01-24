module Lambda.STLC.Term

import Data.Fin
import Data.List
import Control.Isomorphism
import Lambda.STLC.Ty

%default total
%access public export

isoNatUnits : Iso Nat (List ())
isoNatUnits = 
  MkIso 
    (\n => replicate n ()) 
    length
    lem1
    lem2
  where
  lem1 : (y : List ()) -> replicate (length y) () = y
  lem1 [] = Refl
  lem1 (()::xs) = cong $ lem1 xs
  lem2 : (x : Nat) -> length (replicate x ()) = x
  lem2 Z = Refl
  lem2 (S x) = cong $ lem2 x

data Term : List Ty -> Ty -> Type where
  Var : Elem a g -> Term g a 
  Lam : Term (a::g) b -> Term g (a~>b)
  App : Term g (a~>b) -> Term g a -> Term g b

-- map to untyped by forgetting type indices

elem2Nat : Elem a g -> Nat
elem2Nat  Here      = Z
elem2Nat (There el) = S (elem2Nat el)

elem2Fin : Elem a g -> Fin (length g)
elem2Fin  Here      = FZ
elem2Fin (There el) = FS (elem2Fin el)

Show (Term g a) where
  show (Var n) = show $ elem2Nat n
  show (Lam t) = "^" ++ show t
  show (App t u) = "(" ++ show t ++ ")(" ++ show u ++ ")"

-- test terms

TestTy : Ty
TestTy = A~>A

ResultTm : Term [] TestTy
ResultTm = Lam $ Var Here  

TestTm1 : Term [] TestTy
TestTm1 = App (App (Lam $ Var Here) (Lam $ Var Here)) (Lam $ Var Here)

private
TestTm2 : Term [] TestTy
TestTm2 = App (Lam $ Var Here) (App (Lam $ Var Here) (Lam $ Var Here))
