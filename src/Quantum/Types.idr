module Quantum.Types

import Control.Linear.LIO

import Data.String

import Matrix

import Util

export 
Q : Type -> Type 
Q = L1 IO

public export
QSystem : Nat -> Type 
QSystem n = Matrix n 1 Double

public export 
QCircuit : Nat -> Type 
QCircuit n = (1 _ : QSystem n) -> Q (QSystem n)

export 
runCircuit : QSystem n -> QCircuit n -> IO (QSystem n)
runCircuit initial prog = run $ do
    MkMat result <- prog initial
    pure (MkMat result)

export 
finish : (1 q : QSystem n) -> Q (QSystem n)
finish = pure1

export 
zero : Matrix 2 1 Double 
zero = colVect [1, 0]

export 
zeroT : Matrix 1 2 Double
zeroT = rowVect [1, 0]

export 
one : Matrix 2 1 Double 
one = colVect [0, 1]

export
oneT : Matrix 1 2 Double 
oneT = rowVect [0, 1]

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