import Data.List

-- function that decides if class happens or not
isClass k a = (<k) $ length $ takeWhile (<=0) $ sort a 

-- converts a Bool to a string ("YES" or "NO")
bool2Str :: Bool -> String 
bool2Str True = "YES"
bool2Str _ = "NO"

readIntList :: IO [[Int]]
readIntList = 
    do temp <- getContents
       let convertLine t = map read $ words t in
          return $ map convertLine $ tail $ lines temp

solve :: [[Int]] -> [String]
solve (x1:a:xs) = let
                   k = last x1
                   in
                   bool2Str (isClass k a) : (solve xs)
solve _ = []

main = do
    temp <- readIntList
    let t = solve temp
    putStr $ unlines t