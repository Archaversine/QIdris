# QIdris

A Quantum simulator built in Idris 2.

## Description 

This project is a quantum simulator built in Idris 2. It is a work in progress and is not yet complete. The goal is to build a quantum simulator that can simulate quantum circuits and quantum algorithms with type safety via quantitative type theory.

## Example

```idris
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
    let initial = zero >< zero -- |00> via tensor product
        result = prog initial
        probs  = result * result -- Square state vec to get probabilities

    -- Print the probabilities of each state
    putStrLn "Probabilities: "
    putStrLn "-------------------"
    printStateVec probs
```

### Explanation 

The above code defines a quantum program that takes in a quantum circuit of 2 qubits and returns a quantum circuit of 2 qubits. The program puts the first qubit in superposition, then entangles the two qubits by flipping the second qubit if the first qubit is true. The probabilities of each state are then printed. 

### Output

```
Probabilities: 
-------------------
|00> : [0.4999999999999999]
|01> : [0.0]
|10> : [0.0]
|11> : [0.4999999999999999]
```

Here, the output shows that the result of the quantum program is a superposition of the states |00> and |11> with equal probabilities.