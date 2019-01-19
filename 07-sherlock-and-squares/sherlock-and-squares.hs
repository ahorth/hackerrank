solve :: Integer -> Integer -> Int
solve lower_bound upper_bound = length $ takeWhile (>=lower_bound) $ reverse $ takeWhile (<=upper_bound) $ map (\x -> x*x) [(1::Integer)..]

parseIO :: String -> [[Integer]]
parseIO raw = let convertLine t = map read $ words t in  
                map convertLine $ tail $ lines raw

solves :: [[Integer]] -> [Int]
solves ((lower:upper:[]) : xs) = solve lower upper : solves xs
solves _ = []

main = do
    temp <- getContents
    putStr $ unlines $ map show $ solves $ parseIO temp