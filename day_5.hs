module Main where
import Data.List.Split
import Data.List

main :: IO ()
main = do
	input <- readFile("day_5.in")
	let lines = [line | line <- (splitOn "\n" input), length line /= 0]
	let total = length [line | line <- lines, nice line]
	putStrLn $ "Part 1: " ++ show total

nice :: String -> Bool
nice s = ((length vowels) >= 3) && ((length bad) == 0) && ((length doubles) >= 1)
	where
		vowels = [s | v <- s, elem v ['a','e','i','o','u']]
		bad = [x | x <- ["ab", "cd", "pq", "xy"], isInfixOf x s]
		doubles = [d | d <- (map (replicate 2) ['a'..'z']), isInfixOf d s]
