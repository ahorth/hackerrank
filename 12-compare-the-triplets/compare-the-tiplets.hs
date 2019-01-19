
solve as bs =   [alice, bob] where
                    alice = length $ filter (==True) $ zipWith (>) as bs
                    bob =   length $ filter (==True) $ zipWith (<) as bs

readIntList :: IO [Int]
readIntList = do 
    line <- getLine
    return $ map read $ words line

main :: IO ()
main = do 
    alice <- readIntList
    bob   <- readIntList
    let sol  = solve alice bob
    putStrLn $ unwords $ map show sol