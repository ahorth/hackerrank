import Data.List

unfairness :: Int -> Int -> Int
unfairness a b = b - a

solve :: Maybe (Int, [Int]) -> Int
solve (Just (k, xs)) = minimum $ zipWith unfairness sorted (drop (k-1) sorted) where
    sorted = sort xs
solve Nothing = 0

readStr :: String -> [Int]
readStr xs = map read $ words xs

main :: IO ()
main = do
    _ <- getLine
    content <- getContents
    let parseContent = readStr content
    let solution = solve $ uncons parseContent
    putStr (show solution)