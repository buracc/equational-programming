module Practicum3 where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp (Lit n) = show n
showintexp (Add a b) = "(" ++ showintexp a ++ "+" ++ showintexp b ++ ")"
showintexp (Mul a b) = "(" ++ showintexp a ++ "*" ++ showintexp b ++ ")"

evalintexp :: IntExp -> Int
evalintexp (Lit n) = n
evalintexp (Add (Lit a) (Lit b)) = a + b
evalintexp (Mul (Lit a) (Lit b)) = a * b


-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm S = "S"
showterm K = "K"
showterm I = "I"
showterm (App x y) = "(" ++ showterm x ++ showterm y ++ ")"

{-
IP → P
KPQ → P
SPQR → (PR) (QR)
-}

isredex :: Term -> Bool
isredex (App I p) = True
isredex (App (App K p) q) = True
isredex (App (App (App S p) q) r) = True
isredex term = False

isnormalform :: Term -> Bool
isnormalform I = True
isnormalform K = True
isnormalform S = True

headstep :: Term -> Term
headstep = undefined

-- Exercises Equational Specifications
data Thing = Undefined1
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt = undefined

-- 
data I = Undefined2
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s = undefined

p :: I -> I
p = undefined

