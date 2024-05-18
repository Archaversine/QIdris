module Main

import Data.Vect

import Matrix

import Quantum.Gates
import Quantum.Types

-- Define a quantum program that takes in a circuit of 2 qubits
-- 2 qubits = 4 possible states
-- The input matrix is a linear type to prevent copy bugs
prog : (1 m : QCircuit 4) -> QCircuit 4
prog initial = do 
    -- Put the first qubit in superposition (and identity on second)
    let afterH = gate (H >< ID) initial

    -- If the first bit is true, flip the second bit
    -- (this entangles the two bits)
    let afterCNOT = gate (CNOT) afterH

    -- Return the final state
    afterCNOT

main : IO ()
main = do 
    -- Create initial state (both bits set to 0 by default)
    let initial = zero >< zero
        result = prog initial
        probs  = result * result -- Square state vec to get probabilities

    -- Print the probabilities of each state
    putStrLn "Probabilities: "
    putStrLn "-------------------"
    printStateVec probs

