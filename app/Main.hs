module Main where

import Quantum
import PauliOperator
import StabilizerSimulation

import System.Random (StdGen, getStdGen)
import System.Environment (getArgs)
import System.CPUTime
import Control.DeepSeq
import Text.Printf

main :: IO ()
main = do
    gen <- getStdGen
    args <- getArgs
    let fname = fileName args
    contents <- readFile (inputDirectory ++ fname ++ ".chpqasm.txt")
    let circuit = createCircuit (lines contents)
    -- print circuit
    ((tableau, results), gen') <- timeFunction (simulateCircuit circuit) gen
    -- print results
    -- print tableau
    writeFile (outputDirectory ++ fname ++ ".txt") $ unlines (outputMeasureResults results)

-------------
-- Simulation
-------------
simulateCircuit :: Circuit -> StdGen -> ((Tableau, [MeasureResult]), StdGen)
simulateCircuit Circuit {qubitNum' = n, instructionNum = t, instructions = ops} gen = runInstructions ops (tab, []) gen
    where tab = initialTableau n 

runInstructions :: [QuantumInstruction] -> (Tableau, [MeasureResult]) -> StdGen -> ((Tableau, [MeasureResult]), StdGen)
runInstructions [] tabMeas gen = (tabMeas, gen)
runInstructions (qi:qis) tabMeas gen = runInstructions qis tabMeas' gen'
    where (tabMeas', gen') = runInstruction qi tabMeas gen

runInstruction :: QuantumInstruction -> (Tableau, [MeasureResult]) -> StdGen -> ((Tableau, [MeasureResult]), StdGen)
runInstruction Skip (tab, measRes) gen = ((tab, measRes), gen)
runInstruction (QuantumOp g) (tab, measRes) gen = (((applyGate' g tab), measRes), gen)
runInstruction (ClassicOp m) (tab, measRes) gen = ((tab', res:measRes), gen') -- New results first (Stack)
    where ((tab', res), gen') = measure m tab gen

-----------------------------
-- Circuit creation from file
-----------------------------
inputDirectory = "./benchmarks/chpqasm/input/"
outputDirectory = "./benchmarks/chpqasm/output/"

fileName :: [String] -> String
fileName args =
    case args of
        [fname] -> fname
        _       -> error $ "no file name provided."

createCircuit :: [String] -> Circuit
createCircuit (header : qubitLine : instructionLines) 
    | checkHeader header = Circuit { qubitNum' = n, instructionNum = t, instructions = instrs}
    | otherwise          = error $ "Unrecognized file: " ++ header
    where
        n = createQubitNum qubitLine
        instrs' = map createInstruction instructionLines
        instrs = filter (/= Skip) instrs'
        t = length instrs

checkHeader :: String -> Bool
checkHeader h = (h == "CHPQASM")

createQubitNum :: String -> Int
createQubitNum line =
    case words line of
        ["qreg", n] -> read n
        _           -> error $ "Unrecognized line: " ++ line

createInstruction :: String -> QuantumInstruction
createInstruction line =
  case words line of
    ["cx", a, b]   -> QuantumOp (CX {control = (read a), target = (read b)})
    ["h", a]       -> QuantumOp (H {target = (read a)})
    ["s", a]       -> QuantumOp (S {target = (read a)})
    ["measure", a] -> ClassicOp (M {qubit = (read a)})
    _              -> Skip

-------------------------------------
-- Output measurement results to file
-------------------------------------
outputMeasureResults :: [MeasureResult] -> [String]
outputMeasureResults measRes = map outputMeasureResult (reverse measRes)

outputMeasureResult :: MeasureResult -> String
outputMeasureResult MR { qubit' = q, result = res, isRandom = isRand} =
    "Outcome of measuring qubit " ++ show q ++ ": " ++ outputMeasureOutcome res ++ outputMeasureRandomness isRand ++ "\n"

outputMeasureOutcome :: Bool -> String
outputMeasureOutcome False = "0"
outputMeasureOutcome True = "1"

outputMeasureRandomness :: Bool -> String
outputMeasureRandomness False = ""
outputMeasureRandomness True = "  (random)"

---------------------
-- Timing the process
---------------------
timeFunction :: NFData b => (a -> b) -> a -> IO b
timeFunction f x = do
    start <- getCPUTime
    let result = f x
    result `deepseq` return ()  -- force full evaluation
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) :: Double
    printf "Time taken: %.6f seconds\n" diff
    return result