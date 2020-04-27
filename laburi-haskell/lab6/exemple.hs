import Debug.Trace
import System.Random

--my_map :: (a -> b) -> [a] -> [b]

my_map f [] = []
my_map f (x:xs) = (f x) : (map f xs)


my_filter fucnt [] = []
my_filter funct (x:xs) |funct x = x : my_filter funct xs
                       | otherwise = my_filter funct xs



