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
