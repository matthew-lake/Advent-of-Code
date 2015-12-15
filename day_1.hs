module Main where

main :: IO ()
main = do
	input <- readFile("day_1.in")
	let part_1 = count '(' input - count ')' input
	let part_2 = basement 0 input
	putStrLn $  "Part 1: " ++ show part_1
	putStrLn $  "Part 2: " ++ show part_2

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x) 

basement :: Int -> String -> Int
basement a string
	| level == -1 = a
	| otherwise = basement (a + 1) string
	where
		sublist = take a $ string
		level = count '(' sublist - count ')' sublist
