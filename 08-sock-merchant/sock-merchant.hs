import Data.List

solve :: [Int] -> Int
solve l = sum $ map  ((`div` 2) . length) $ group $ (sort l)

main = interact $ show .  solve . map read . tail . words