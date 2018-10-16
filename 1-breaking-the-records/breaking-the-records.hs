tally :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
tally  (xh, xl, nh, nl) score | score > xh = (score, xl, nh+1, nl)
                              | score < xl = (xh, score, nh, nl+1)
                              | otherwise  = (xh, xl, nh, nl)

solve :: [Int] -> [Int]
solve []    = [0,0]
solve [_]   = [0,0]
solve (x:xs) =
    let (_, _, nh,nl) = foldl tally (x,x,0,0) xs
    in [nh, nl]

main = interact $ unwords . map show . solve . map read . tail . words