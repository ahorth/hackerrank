main = interact $ show . product . (\n -> [1..n]) . read