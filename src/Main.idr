module Main

import Data.Vect

import Matrix

import Quantum.Gates
import Quantum.Types

-- Define a quantum program that takes in a circuit of 2 qubits
-- 2 qubits = 4 possible states
-- The input matrix is a linear type to prevent copy bugs
prog : QCircuit 4
prog initial = do 
    let result = initial
          |> (H >< ID) -- Put the first qubit in superposition (and identity on second)
          |> CNOT      -- If the first bit is true, flip the second bit (entangles the two bits)

    -- Return the final state
    finish result

main : IO ()
main = do 
    -- Create initial state (both bits set to 0 by default)
    let initial = zero >< zero
    
    result <- runCircuit initial prog
    let probs  = result * result -- Square state vec to get probabilities

    -- Print the probabilities of each state
    putStrLn "Probabilities: "
    putStrLn "-------------------"
    printStateVec result
    printStateVec probs
