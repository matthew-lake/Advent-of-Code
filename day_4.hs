module Main where
import Data.List.Split
import Data.List
import Data.Hash.MD5

main :: IO ()
main = do
	let partOne = takeWhile (\x -> not $ checkZeroes 5 (input ++ x)) [show a | a <- [0..]]
	putStrLn . show $ ((read (last partOne) :: Int) + 1)

	let partTwo = takeWhile (\x -> not $ checkZeroes 6 (input ++ x)) [show a | a <- [0..]]
	putStrLn . show $ ((read (last partTwo) :: Int) + 1)
	where
		input = "ckczppom"

checkZeroes :: Int -> String -> Bool
checkZeroes x a = (take x . md5s . Str $ a) == replicate x '0'
