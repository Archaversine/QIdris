module Quantum.Gates

import Control.Linear.LIO

import Data.Vect

import Matrix

import Quantum.Types

import System.Random

export
gate : {c1 : Nat} -> {n : Nat} -> Num a => (1 g : Matrix r1 c1 a) -> (1 m : Matrix c1 n a) -> Matrix r1 n a
gate (MkMat g) (MkMat m) = matmul (MkMat g) (MkMat m)

export infixl 4 |>

export
(|>) : {c1 : Nat} -> {n : Nat} -> Num a => (1 m : Matrix c1 n a) -> (1 g : Matrix r1 c1 a) -> Matrix r1 n a
(|>) (MkMat m) (MkMat g) = matmul (MkMat g) (MkMat m)

public export
ID : Matrix 2 2 Double
ID = identity

public export
IDN : (n : Nat) -> Matrix n n Double
IDN n = identity

public export
X : Matrix 2 2 Double
X = MkMat [[0, 1], [1, 0]]

public export
H : Matrix 2 2 Double
H = (MkMat [[1, 1], [1, -1]]).scale (1 / sqrt 2)

public export 
CNOT : Matrix 4 4 Double
CNOT = MkMat [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

public export
pow2 : Num a => Nat -> a
pow2 0 = 1
pow2 (S n) = 2 * pow2 n

-- See https://quantumcomputing.stackexchange.com/questions/4252/how-to-derive-the-cnot-matrix-for-a-3-qubit-system-where-the-control-target-qu
public export 
CNOTN : (n : Nat) -> Matrix (pow2 (n `minus` 1)) (pow2 (n `minus` 1)) Double -> Matrix (2 * pow2 (n `minus` 1)) (2 * pow2 (n `minus` 1)) Double
CNOTN n m = (zero `matmul` zeroT) >< identity {size=pow2 (n `minus` 1)} + (one `matmul` oneT) >< m

measureMatPure : (mes : Double) -> Vect n (Vect 1 Double) -> Vect n (Vect 1 Double)
measureMatPure k [] = []
measureMatPure k [[x]] = [[1]]
measureMatPure k (x :: xs) with (k <= head x * head x)
    _ | True  = rewrite sym (lengthCorrect xs) in [1] :: replicate (length xs) [0]
    _ | False = [0] :: measureMatPure k xs

||| Deterministically measure a quantum system
export
measurePure : (mes : Double) -> (1 q : QSystem n) -> QSystem n
measurePure mes (MkMat contents) = MkMat (measureMatPure mes contents)

||| Non-deterministically measure a quantum system
export
measure : (1 q : QSystem n) -> Q (QSystem n)
measure (MkMat contents) = do 
    k <- randomRIO {a=Double} (0, 1)
    pure1 (MkMat (measureMatPure k contents))