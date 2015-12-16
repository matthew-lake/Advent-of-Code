module Main where
import Data.List.Split
import Data.List

main :: IO ()
main = do
	input <- readFile("day_3.in")
	let cleanInput = [c | c <- input, c == '<' || c == '>' || c == 'v' || c == '^']

	let sublists = [take x cleanInput | x <- [1..(length cleanInput)]]
	let visited = nub [position s (0,0) | s <- sublists]
	putStrLn $ "Part 1: " ++ show (length visited)	

	let santa = [cleanInput !! x | x <- [0..((length cleanInput) - 1)], odd x]
	let robot = [cleanInput !! x | x <- [0..((length cleanInput) - 1)], even x]
	let visitedSanta = nub [position s (0,0) | s <- [take x santa | x <- [1..(length santa)]]]
	let visitedRobot = nub [position s (0,0) | s <- [take x robot | x <- [1..(length robot)]]]
	let both = nub (visitedSanta ++ visitedRobot)
	putStrLn $ "Part 2: " ++ show (length both)

position :: [Char] -> (Int, Int) -> (Int, Int)
position [c] (a,b)
	| c == '<' = (a - 1, b)
	| c == '>' = (a + 1, b)
	| c == 'v' = (a, b - 1)
	| c == '^' = (a, b + 1)
position (c:cs) (a,b)
	| c == '<' = position cs (a - 1, b)
	| c == '>' = position cs (a + 1, b)
	| c == 'v' = position cs (a, b - 1)
	| c == '^' = position cs (a, b + 1)
