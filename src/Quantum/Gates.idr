module Quantum.Gates

import Data.Vect

import Matrix

import Quantum.Types

export
gate : {c1 : Nat} -> {n : Nat} -> Num a => (1 _ : Matrix r1 c1 a) -> (1 _ : Matrix c1 n a) -> Matrix r1 n a
gate (MkMat m1) (MkMat m2) = matmul (MkMat m1) (MkMat m2)

public export
ID : Matrix 2 2 Double
ID = MkMat [[1, 0], [0, 1]]

public export
X : Matrix 2 2 Double
X = MkMat [[0, 1], [1, 0]]

public export
H : Matrix 2 2 Double
H = (MkMat [[1, 1], [1, -1]]).scale (1 / sqrt 2)

public export 
CNOT : Matrix 4 4 Double
CNOT = MkMat [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]
