
slope: Nat -> List (Nat, String) -> List (Nat, String)
slope n inp = map (\lineWithCount => ((n * (fst lineWithCount)) `mod` (length (snd lineWithCount)), snd lineWithCount)) inp

trees: List (Nat, String) -> Nat
trees xs = length $ findIndices (\(n, line) => (strIndex line (fromNat n) == '#')) xs

slopeTrees: Nat -> List (Nat, String) -> Nat
slopeTrees n xs = trees $ slope n xs

slope2: List (Nat, String) -> List (Nat, Nat, String)
slope2 inp = map (\lineWithCount => (fst lineWithCount, ((fst lineWithCount) `div` 2) `mod` (length (snd lineWithCount)), snd lineWithCount)) inp

trees2: List (Nat, Nat, String) -> Nat
trees2 xs = length $ findIndices (\(n, idx, line) => n `mod` 2 == 0 && strIndex line (fromNat idx) == '#') xs

slopeTrees2: List (Nat, String) -> Nat
slopeTrees2 xs = trees2 $ slope2 xs

main: IO ()
main = do Right input <- readFile "Inputs/day3.txt" | Left err => putStrLn (show err)
          let allLines = lines input
          let zipped = zip [0..(length allLines)] allLines
          putStrLn $ show (slopeTrees 1 zipped)
          putStrLn $ show (slopeTrees 3 zipped)
          putStrLn $ show (slopeTrees 5 zipped)
          putStrLn $ show (slopeTrees 7 zipped)
          putStrLn $ show (slope2 zipped)
          putStrLn $ show (slopeTrees2 zipped)
          let trees = (slopeTrees 1 zipped) * (slopeTrees 3 zipped) * (slopeTrees 5 zipped) * (slopeTrees 7 zipped) * (slopeTrees2 zipped)
          putStrLn $ show trees
