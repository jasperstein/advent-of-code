record PwSpec where 
    constructor MkPwSpec
    min: Nat
    max: Nat
    char: Char
    pw: String

parts: String -> Maybe (String, String, String)
parts x = case split (==' ') x of
    [a,b,c] => Just (a,b,c)
    _ => Nothing

parseRange: String -> Maybe (Nat, Nat)
parseRange x with (split (=='-') x)
  parseRange x | (y :: (z :: [])) = Just ((cast {to = Nat} y), (cast {to = Nat} z))
  parseRange x | _ = Nothing

parseChar: String -> Maybe Char
parseChar x = case (unpack x) of
    [a, ':'] => Just a
    _ => Nothing

parsePwSpec: String -> Maybe PwSpec
parsePwSpec line = do
    (range, charSpec, pw) <- parts line
    (min, max) <- parseRange range
    char <- parseChar charSpec
    pure (MkPwSpec min max char pw)

checkPw: PwSpec -> Bool
checkPw (MkPwSpec min max char pw) = 
    let count = length (filter (==char) (unpack pw)) in
      elem count [min .. max]

checkPw2: PwSpec -> Bool
checkPw2 (MkPwSpec min max char pw) = ((strIndex pw $ (fromNat min) - 1) == char) /= ((strIndex pw $ (fromNat max) - 1) == char)

count: (PwSpec -> Bool) -> List (Maybe PwSpec) -> Int
count isValid xs = 
    foldl (\i,mpw =>
        case mpw of
            Just x => if isValid x then i+1 else i
            _ => i)
        0
        xs
              
main: IO ()
main = do Right input <- readFile "Inputs/day2.txt" | Left err => putStrLn (show err)
          let pwds = map parsePwSpec (lines input)
          let count1 = count checkPw pwds
          let count2 = count checkPw2 pwds
          putStrLn (show count1)
          putStrLn (show count2)