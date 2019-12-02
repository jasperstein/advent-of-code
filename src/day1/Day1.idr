module day1.Main

modules : String -> List Integer
modules input = map cast (lines input)

fuel: (mass: Integer) -> Integer
fuel mass = 0 `max` pred (pred (mass `div` 3))

fuelStream: (mass: Integer) -> List Integer
fuelStream mass = let theFuel = fuel mass in 
    theFuel :: case theFuel of 
        0 => []
        _ => fuelStream theFuel
        
totalFuel: (mass: Integer) -> Integer
totalFuel mass = sum (fuelStream mass)


main : IO ()
main = do
    Right input <- Prelude.File.readFile "src/day1/Input.txt" | Left err => putStrLn "FileError"
    let moduleWeights = modules input
    putStrLn . show . sum $ map fuel moduleWeights -- part 1
    putStrLn . show . sum $ map totalFuel moduleWeights -- part 2
    