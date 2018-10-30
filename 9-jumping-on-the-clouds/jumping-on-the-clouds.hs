solve :: Int -> [Int]-> Int

solve count []   = count
solve count [x]  = count + 1
solve count (x1:x2:xs) |x1 == 1   = solve (count+1) xs 
                       |x2 == 1   = solve (count+1 )(x2:xs)
                       |otherwise = solve (count+1) xs 

main = interact $ show . solve 0 . map read . tail . tail . words