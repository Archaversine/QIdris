module Examples

import Quantum

-- Classical bit flip 
classicalFlip : QCircuit 2 
classicalFlip initial = finish (initial |> X)

-- Superposition (50% chance of 0, 50% chance of 1)
superPos : QCircuit 2 
superPos initial = measure (initial |> H)

-- Classical conditional flip
-- First bit: control 
-- Second bit: target
classicalCnot : QCircuit 4 
classicalCnot initial = finish (initial |> CNOT)

-- Superposition conditional flip
-- First bit: control
-- Second bit: target
-- This creates entanglement between the control and target qubits
quantumCnot : QCircuit 4 
quantumCnot initial = measure $ initial 
                             |> H >< ID
                             |> CNOT


-- CNOT on non adjacent qubits
-- First bit: control bit
-- Third bit: target bit
-- (the second bit is not used)
cnot3 : QCircuit 8 
cnot3 initial = measure $ initial 
                       |> H >< IDN 4
                       |> CNOTN 3 (ID >< X)

-- CNOT with multiple target bits 
-- First bit: control bit
-- Second bit: target bit 1
-- Third bit: target bit 2
cnotMulti : QCircuit 8 
cnotMulti initial = finish $ initial 
                          |> H >< IDN 4 
                          |> CNOTN 3 (X >< X)

