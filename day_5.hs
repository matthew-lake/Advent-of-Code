module Main where
import Data.List.Split
import Data.List

main :: IO ()
main = do
	input <- readFile("day_5.in")
	let lines = [line | line <- (splitOn "\n" input), length line /= 0]
	let partOne = length [line | line <- lines, nice line]
	putStrLn $ "Part 1: " ++ show partOne

	let partTwo = length [line | line <- lines, nicer line]
	putStrLn $ "Part 2: " ++ show partTwo

nice :: String -> Bool
nice s = ((length vowels) >= 3) && ((length bad) == 0) && ((length doubles) >= 1)
	where
		vowels = [s | v <- s, elem v ['a','e','i','o','u']]
		bad = [x | x <- ["ab", "cd", "pq", "xy"], isInfixOf x s]
		doubles = [d | d <- (map (replicate 2) ['a'..'z']), isInfixOf d s]

nicer :: String -> Bool
nicer s = (pair s) && (gap s)
	
pair :: String -> Bool
pair (_:"") = False
pair (_:_:"") = False
pair (_:_:_:"") = False
pair (a:b:s) = do
	if isInfixOf (a:b:"") s
		then True
	else
		pair (b:s)

gap :: String -> Bool
gap (_:"") = False
gap (_:_:"") = False
gap (a:_:b:"") = (a == b)
gap (a:b:c:s) = do
	if (a == c)
		then True
	else
		gap (b:c:s)
