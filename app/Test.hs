import StabilizerSimulation
import PauliOperator
import Quantum

import Data.Bits
import Data.Bit
import Data.BitVector
import System.Random (StdGen, mkStdGen)

h0 = H {target = 0}
h1 = H {target = 1}

s0 = S {target = 0}
s1 = S {target = 1}

cx01 = CX {control = 0, target = 1}
cx10 = CX {control = 1, target = 0}

m0 = M {qubit = 0}
m1 = M {qubit = 1}

xs0 = fromBits [True, False]
zs0 = fromBits [False, False]
r0 = 0 :: Bit
p0 = Pauli {xBits = xs0, zBits = zs0, phaseBit = r0}

xs1 = fromBits [False, True]
zs1 = fromBits [False, False]
r1 = 0 :: Bit
p1 = Pauli {xBits = xs1, zBits = zs1, phaseBit = r1}

xs2 = fromBits [False, False]
zs2 = fromBits [True, False]
r2 = 0 :: Bit
p2 = Pauli {xBits = xs2, zBits = zs2, phaseBit = r2}

xs3 = fromBits [False, False]
zs3 = fromBits [False, True]
r3 = 0 :: Bit
p3 = Pauli {xBits = xs3, zBits = zs3, phaseBit = r3}


destabs0 = [p0, p1]
stabs0 = [p2, p3]

tab0 = Tableau {qubitNum = 2, destabilizers = destabs0, stabilizers = stabs0}

tab1 = applyGate' h0 tab0

((tab2, res2), gen2) = measure m0 tab1 (mkStdGen 21)

tab3 = applyGate' h0 tab2

tab4 = applyGate' cx01 tab3

((tab5, res5), gen5) = measure m1 tab4 gen2
((tab6, res6), gen6) = measure m0 tab5 gen5


simulateCircuit :: Circuit -> StdGen -> ((Tableau, [MeasureResult]), StdGen)
simulateCircuit Circuit {qubitNum' = n, instructionNum = t, instructions = ops} gen = runInstructions ops (tab, []) gen
    where tab = initialTableau n 

runInstructions :: [QuantumInstruction] -> (Tableau, [MeasureResult]) -> StdGen -> ((Tableau, [MeasureResult]), StdGen)
runInstructions [] tabMeas gen = (tabMeas, gen)
runInstructions (qi:qis) tabMeas gen = runInstructions qis tabMeas' gen'
    where (tabMeas', gen') = runInstruction qi tabMeas gen

runInstruction :: QuantumInstruction -> (Tableau, [MeasureResult]) -> StdGen -> ((Tableau, [MeasureResult]), StdGen)
runInstruction (QuantumOp g) (tab, measRes) gen = (((applyGate' g tab), measRes), gen)
runInstruction (ClassicOp m) (tab, measRes) gen = ((tab', res:measRes), gen') -- New results first (Stack)
    where ((tab', res), gen') = measure m tab gen

processLine :: String 

instruction :: String -> Maybe QuantumInstruction
instruction line =
  case words line of
    ["cx", a, b]   -> Just $ QuantumOp (CX {control = (read a), target = (read b)})
    ["h", a]       -> Just $ QuantumOp (H {target = (read a)})
    ["s", a]       -> Just $ QuantumOp (S {target = (read a)})
    ["measure", a] -> Just $ ClassicOp (M {qubit = (read a)})
    _           -> Nothing