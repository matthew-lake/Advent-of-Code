module Main where
import Data.List.Split
import Data.List

main :: IO ()
main = do
	input <- readFile("day_2.in")
	let lines = [line | line <- (splitOn "\n" input), length line /= 0]
	let total = sum [paper line | line <- lines]
	let length = sum [ribbon line | line <- lines]
	putStrLn $ "Part 1: " ++ show total
	putStrLn $ "Part 2: " ++ show length

paper :: String -> Int
paper line = sum [2*side | side <- sides] + minimum sides
	where 
		ints = [read c :: Int | c <- (splitOn "x" line)]
		l = ints !! 0
		w = ints !! 1
		h = ints !! 2
		sides = [l*w, w*h, h*l]

ribbon :: String -> Int
ribbon line = sum [2*side | side <- (drop 1 (reverse (sort ints)))] + bow
	where
		ints = [read c :: Int | c <- (splitOn "x" line)]
		l = ints !! 0
		w = ints !! 1
		h = ints !! 2
		bow = l*w*h
