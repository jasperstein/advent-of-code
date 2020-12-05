data DoubleEnded: List a -> Type where
    Empty: DoubleEnded []
    Singleton: (x: a) -> DoubleEnded [x]
    HeadBodyTail: (hd: a) -> (body: List a) -> (tl: a) -> DoubleEnded (hd :: body ++ [tl])

doubleEnded: (l: List a) -> DoubleEnded l
doubleEnded [] = Empty
doubleEnded (x :: xs) with (doubleEnded xs)
  doubleEnded (x :: []) | Empty = Singleton x
  doubleEnded (x :: [y]) | Singleton y = HeadBodyTail x Nil y
  doubleEnded (x :: (hd :: (body ++ [tl]))) | HeadBodyTail hd body tl = HeadBodyTail x (hd :: body) tl

findN : Int -> List Int -> Maybe (Int, Int, Int)
findN target input with (doubleEnded input)
  findN target [] | Empty = Nothing
  findN target [x] | Singleton x = Nothing
  findN target (hd :: tail@(body ++ [tl])) | HeadBodyTail hd body tl = 
    case compare (hd + tl) target of
        LT => findN target tail
        GT => findN target $ hd :: body
        EQ => Just (hd, tl, hd*tl)

find3: List Int -> Maybe (Int, Int, Int, Int)
find3 [] = Nothing
find3 (x :: xs) with (findN (2020-x) xs)
  find3 (x :: xs) | Nothing = find3 xs
  find3 (x :: xs) | Just (a, b, c) = Just (x, a, b, x*c)


main: IO ()
main = do Right input <- readFile "Inputs/day1.txt" | Left err => putStrLn (show err)
          let l = sort $ map (cast {to = Int}) $ lines input
          putStrLn . show $ findN 2020 l
          putStrLn . show $ find3 l
          
          