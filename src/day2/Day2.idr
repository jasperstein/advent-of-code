module day2.Main


Mem: Type
Mem = List Int

Map: Type -> Type -> Type
Map a b = List (a, b)

lookup: Eq a => Map a b -> a -> Maybe b
lookup [] v = Nothing
lookup ((a, b) :: xs) v = if v==a then Just b else lookup xs v

CmdType: Type
CmdType = (prgCtr: Nat) -> (mem: Mem) -> Maybe Mem

addCmd: (prgCtr: Nat) -> (mem: Mem) -> Maybe Mem
multCmd: (prgCtr: Nat) -> (mem: Mem) -> Maybe Mem
replaceAt: List a -> Nat -> a -> List a
runProg: (start: Mem) -> (opcodes: Map Nat (Nat, CmdType)) -> Maybe Mem
runProg_aux: (mem: Mem) -> (opcodes: Map Nat (Nat, CmdType)) -> (prgCtr: Nat) -> Maybe Mem

opCodes: Map Nat (Nat, CmdType)
opCodes = [(1, (4, addCmd)), (2, (4, multCmd))]

runProg start opc = runProg_aux start opc 0

runProg_aux mem opc prgCtr = do
    nextCmd <- index' prgCtr mem
    if nextCmd == 99 then Just mem else do
        (skip, op) <- lookup opc (toNat nextCmd)
        newMem <- op prgCtr mem
        runProg_aux newMem opc (prgCtr + skip)

addCmd prgCtr mem = do
    idx1 <- index' (prgCtr + 1) mem 
    idx2 <- index' (prgCtr + 2) mem 
    idx3 <- index' (prgCtr + 3) mem 
    val1 <- index' (toNat idx1) mem
    val2 <- index' (toNat idx2) mem
    pure (replaceAt mem (toNat idx3) (val1 + val2))

multCmd prgCtr mem = do
    idx1 <- index' (prgCtr + 1) mem 
    idx2 <- index' (prgCtr + 2) mem 
    idx3 <- index' (prgCtr + 3) mem 
    val1 <- index' (toNat idx1) mem
    val2 <- index' (toNat idx2) mem
    pure (replaceAt mem (toNat idx3) (val1 * val2))
    
replaceAt [] x y = []
replaceAt (z :: xs) Z y = y :: xs
replaceAt (z :: xs) (S i) y = z :: (replaceAt xs i y)

answer2: Mem -> List (Int, Int)
answer2 origMem = do
    i <- [0..99]
    j <- [0..99]
    let mem = (origMem `replaceAt` 1 $ i) `replaceAt` 2 $ j
    case runProg mem opCodes of
        Just finalMem => if index' 0 finalMem == Just 19690720 then [(i,j)] else []
        Nothing => []

main: IO ()
main = do
    Right input <- Prelude.File.readFile "src/day2/Input.txt" | Left err => putStrLn "FileError"
    let origMem = map {b = Int} cast (split (== ',') input)
    let mem = (origMem `replaceAt` 1 $ 12) `replaceAt` 2 $ 2
    let result = runProg mem opCodes
    putStrLn . show $ result
    putStrLn . show $ (answer2 origMem)