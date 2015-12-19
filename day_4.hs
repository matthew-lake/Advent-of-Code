module Main where
import Data.List.Split
import Data.List
import Data.Hash.MD5

main :: IO ()
main = do
	let list = takeWhile (\x -> not . checkZeroes $ (input ++ x)) [show a | a <- [0..]]
	putStrLn . show $ ((read (last list) :: Int) + 1)
	where
		input = "ckczppom"

checkZeroes :: String -> Bool
checkZeroes a = (take 5 . md5s . Str $ a) == "00000"
