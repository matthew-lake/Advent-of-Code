module Main where
import Data.List.Split

main :: IO ()
main = do
	input <- readFile("day_2.in")
	let lines = [line | line <- (splitOn "\n" input), length line /= 0]
	let total = sum [paper line | line <- lines]
	putStrLn $ "Part 1: " ++ show total

paper :: String -> Int
paper line = sum [2*side | side <- sides] + minimum sides
	where 
		ints = [read c :: Int | c <- (splitOn "x" line)]
		l = ints !! 0
		w = ints !! 1
		h = ints !! 2
		sides = [l*w, w*h, h*l]
