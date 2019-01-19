btwenSetsFirst :: [Int] -> Int -> Bool
btwenSetsFirst [] _ = True
btwenSetsFirst (x:xs) j |  (j `mod` x) == 0 = btwenSetsFirst xs j
                        | otherwise = False

btwenSetsSecond :: [Int] -> Int -> Bool
btwenSetsSecond [] _ = True
btwenSetsSecond (x:xs) j |  (x `mod` j) == 0 = btwenSetsSecond xs j
                         | otherwise = False

possibleNums :: [Int] -> [Int] -> [Int]
possibleNums x y = [maxelem..minelem] where
                    maxelem = foldl max 0 x
                    minelem = foldl min 101 y

solve :: [Int] -> [Int] -> Int
solve x y = length $ filter (==True) [(firstList num) && (secondList num) | num <-trials]
            where
            firstList = btwenSetsFirst x
            secondList= btwenSetsSecond y
            trials = possibleNums x y

-- from @tsoding https://youtu.be/40kpc90ZzDg
readIntList :: IO [Int]
readIntList = 
    do line <- getLine
       return $ map read $ words line


main :: IO()
main = do
    _ <- getLine
    x <- readIntList
    y <- readIntList
    putStrLn $ show $ solve x y 