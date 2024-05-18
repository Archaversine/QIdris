module Quantum.Types

import public Control.Linear.LIO

import Data.String

import Matrix

import Util

||| The Monad in which quantum computations are performed.
public export 
Q : Type -> Type 
Q = L1 IO

||| A quantum system of size `n` where `n` represents the 
||| number of possible states in the system.
|||
||| For a quantum system of 3 qubits, `n` would be 8.
||| because 2^3 = 8.
public export
QSystem : Nat -> Type 
QSystem n = Matrix n 1 Double

||| A quantum circuit is defined as a function that applies
||| a transformation to a quantum system.
|||
||| NOTE: The input to the circuit is a linear argument.
public export 
QCircuit : Nat -> Type 
QCircuit n = (1 _ : QSystem n) -> Q (QSystem n)

||| Run a quantum circuit with a given initial system state.
|||
||| Example: runCircuit myCircuit (one >< zero >< zero)
||| runs `myCircuit` with the initial state |100>.
export 
runCircuit : QSystem n -> QCircuit n -> IO (QSystem n)
runCircuit initial prog = run $ do
    MkMat result <- prog initial
    pure (MkMat result)

||| Finish a quantum circuit by returning the given system state.
export 
finish : (1 q : QSystem n) -> Q (QSystem n)
finish = pure1

||| Equivalent to |0>, or the column vector [1, 0].
export 
zero : Matrix 2 1 Double 
zero = colVect [1, 0]

||| Equivalent to <0|, or the row vector [1, 0].
export 
zeroT : Matrix 1 2 Double
zeroT = rowVect [1, 0]

||| Equivalent to |1>, or the column vector [0, 1].
export 
one : Matrix 2 1 Double 
one = colVect [0, 1]

||| Equivalent to <1|, or the row vector [0, 1].
export
oneT : Matrix 1 2 Double 
oneT = rowVect [0, 1]

toBinString : Nat -> String
toBinString x = pack (reverse (toBinString' x))
    where 
        odd : Nat -> Bool
        odd Z = False 
        odd (S Z) = True
        odd (S n) = not (odd n)

        toBinString' : Nat -> List Char
        toBinString' 0 = []
        toBinString' x = (if odd x then '1' else '0') :: toBinString' (x `div` 2)

diracStrings : (n : Nat) -> NonZero n => Vect n String 
diracStrings n = do 
    let unpadded = map (toBinString . finToNat) (range {len=n})
        longest  = foldl maximum 0 (map length unpadded)
        padded   = map (padLeft longest '0') unpadded
        notated  = map (\x => "|" ++ x ++ ">") padded

    notated

||| Convert a column state vector to a string representation.
||| Example: showStateVec zero = 
||| |0> : [1.0]
||| |1> : [0.0]
export
showStateVec : {n : Nat} -> NonZero n => Matrix n 1 Double -> String
showStateVec (MkMat m) = do 
    let diracs = diracStrings n
        showed = zipWith (\d, v => d ++ " : " ++ show v) diracs m

    unlines showed

||| Prints a column state vector to the console. See `showStateVec`.
export 
printStateVec : HasIO io => {n : Nat} -> NonZero n => Matrix n 1 Double -> io ()
printStateVec = putStrLn . showStateVec