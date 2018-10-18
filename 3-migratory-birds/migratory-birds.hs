countBirds :: [Int] -> [Int] -> [Int]
countBirds [] counts = counts
countBirds (x:xs) [b1,b2,b3,b4,b5] | x == 1 = countBirds xs [b1+1,b2,b3,b4,b5]
                                   | x == 2 = countBirds xs [b1,b2+1,b3,b4,b5]
                                   | x == 3 = countBirds xs [b1,b2,b3+1,b4,b5]
                                   | x == 4 = countBirds xs [b1,b2,b3,b4+1,b5]
                                   | x == 5 = countBirds xs [b1,b2,b3,b4,b5+1]
                                   | otherwise = countBirds xs [b1+1,b2,b3,b4,b5]

solve :: [Int] -> Maybe Int
solve listbirds =  let
                   count_birds = countBirds listbirds [0,0,0,0,0]
                   max_count = maximum count_birds
                   in 
                   lookup max_count $ zip count_birds [1,2,3,4,5]

solve_maybe_lift :: Maybe Int -> Int
solve_maybe_lift (Just a) = a
solve_maybe_lift Nothing = 0

main = interact $ show . solve_maybe_lift . solve . map read . tail . words