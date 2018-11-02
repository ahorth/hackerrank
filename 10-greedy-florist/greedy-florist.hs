import Data.List

type Mult = Int
type Tab = Int

type Person = (Mult, Tab)
-- flower_cost, multiplier, tab
updatePerson :: Int -> Person -> Person
updatePerson price  (m, tab)= (m+1, tab + m*price)

-- list of flower prices, list of people 
updatePersons:: [Int] -> [Person] -> [Person]
updatePersons [] ps = ps
updatePersons (f:fs) (p:ps) = updatePersons fs $ ps ++ [updatePerson f p]

totalCost:: [Person] -> Int
totalCost [] = 0
totalCost ps = sum $ snd $ unzip ps

solve k flowers = totalCost $ updatePersons flowers $ replicate k (1, 0)

main = do
    temp <- getLine
    let k = (!!1) $ map read $ words temp :: Int
    temp_prices <- getLine
    let prices = map read $ words temp_prices :: [Int]
    putStr $ show $ solve k $ reverse $ sort prices