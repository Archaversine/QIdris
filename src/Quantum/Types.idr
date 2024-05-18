module Quantum.Types

import Data.String

import Matrix

import Util

public export
QCircuit : Nat -> Type 
QCircuit n = Matrix n 1 Double

export 
zero : Matrix 2 1 Double 
zero = fromVect [1, 0]

export 
one : Matrix 2 1 Double 
one = fromVect [0, 1]

odd : Nat -> Bool
odd Z = False 
odd (S Z) = True
odd (S n) = not (odd n)

toBinString' : Nat -> List Char
toBinString' 0 = []
toBinString' x = (if odd x then '1' else '0') :: toBinString' (x `div` 2)

toBinString : Nat -> String
toBinString x = pack (reverse (toBinString' x))

export
diracStrings : (n : Nat) -> NonZero n => Vect n String 
diracStrings n = do 
    let unpadded = map (toBinString . finToNat) (range {len=n})
        longest  = foldl maximum 0 (map length unpadded)
        padded   = map (padLeft longest '0') unpadded
        notated  = map (\x => "|" ++ x ++ ">") padded

    notated

export
showStateVec : {n : Nat} -> NonZero n => Matrix n 1 Double -> String
showStateVec (MkMat m) = do 
    let diracs = diracStrings n
        showed = zipWith (\d, v => d ++ " : " ++ show v) diracs m

    unlines showed

export 
printStateVec : HasIO io => {n : Nat} -> NonZero n => Matrix n 1 Double -> io ()
printStateVec = putStrLn . showStateVec