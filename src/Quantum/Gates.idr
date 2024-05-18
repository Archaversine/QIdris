module Quantum.Gates

import Control.Linear.LIO

import Data.Vect

import public Matrix

import Quantum.Types

import System.Random

||| Apply a quantum gate to a quantum system via matrix multiplication.
||| Example: `gate (H >< ID) myVec`
||| Note: Both inputs to `gate` are linear.
export
gate : {c1 : Nat} -> {n : Nat} -> Num a => (1 g : Matrix r1 c1 a) -> (1 m : Matrix c1 n a) -> Matrix r1 n a
gate (MkMat g) (MkMat m) = matmul (MkMat g) (MkMat m)

export infixl 4 |>

||| An infix operator for `gate`.
||| m |> g = gate g m
|||
||| This is useful for chaining gates together.
export
(|>) : {c1 : Nat} -> {n : Nat} -> Num a => (1 m : Matrix c1 n a) -> (1 g : Matrix r1 c1 a) -> Matrix r1 n a
(|>) (MkMat m) (MkMat g) = matmul (MkMat g) (MkMat m)

||| The Identity gate. This gate does nothing.
public export
ID : Matrix 2 2 Double
ID = identity

||| The Identity gate of size `n`. where `n` is the rows and cols of the matrix.
public export
IDN : (n : Nat) -> Matrix n n Double
IDN n = identity

||| The Pauli-X gate. This gate flips the state of a qubit.
||| This is equivalent to the classical NOT gate.
public export
X : Matrix 2 2 Double
X = MkMat [[0, 1], [1, 0]]

||| The Hadamard Gate.
||| This gate acts upon a single qubit and puts it into a superposition.
public export
H : Matrix 2 2 Double
H = (MkMat [[1, 1], [1, -1]]).scale (1 / sqrt 2)

||| Controlled Not Gate.
||| This gate flips the second qubit if the first qubit is in the state |1>.
|||
||| NOTE: This gate is only defined for 2 qubits that are adjacent to each other.
||| Use `CNOTN` for non adjacent qubits.
public export 
CNOT : Matrix 4 4 Double
CNOT = MkMat [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

pow2 : Num a => Nat -> a
pow2 0 = 1
pow2 (S n) = 2 * pow2 n

||| Controlled Not Gate for non-adjacent qubits.
||| See https://quantumcomputing.stackexchange.com/questions/4252/how-to-derive-the-cnot-matrix-for-a-3-qubit-system-where-the-control-target-qu
public export 
CNOTN : (n : Nat) -> Matrix (pow2 (n `minus` 1)) (pow2 (n `minus` 1)) Double -> Matrix (2 * pow2 (n `minus` 1)) (2 * pow2 (n `minus` 1)) Double
CNOTN n m = (zero `matmul` zeroT) >< identity {size=pow2 (n `minus` 1)} + (one `matmul` oneT) >< m

measureMatPure : (mes : Double) -> Vect n (Vect 1 Double) -> Vect n (Vect 1 Double)
measureMatPure k [] = []
measureMatPure k [[x]] = [[1]]
measureMatPure k (x :: xs) with (k <= head x * head x)
    _ | True  = rewrite sym (lengthCorrect xs) in [1] :: replicate (length xs) [0]
    _ | False = [0] :: measureMatPure (k - head x) xs

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