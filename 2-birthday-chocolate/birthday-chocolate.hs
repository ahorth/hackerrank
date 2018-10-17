takeflush :: Int -> [a] -> [[a]]
takeflush 0 _ = []
takeflush _ []= []
takeflush n l | length l < n = []
              | otherwise = (take n l) : takeflush n (tail l)

-- m is how many we take, d is what the sum must be equal to, s is the  array of ints.

solve :: Int -> Int -> [Int] -> Int
solve d m s = length $ filter (==d) $ map sum $ takeflush m s

-- from @tsoding https://youtu.be/40kpc90ZzDg
-- still hazy on how haskell does IO, so stole this readIntList.
readIntList :: IO [Int]
readIntList = 
    do line <- getLine
       return $ map read $ words line

main = do
    _ <- getLine
    s <- readIntList
    [d,m] <- readIntList
    putStrLn $ show $ solve d m s