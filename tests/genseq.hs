import Data.List (permutations)

main = putStrLn $ concatMap (\(x,y) -> x++y) (xs `zip` repeat "-")
    where xs = concat $ permutations ["a", "a", "i", "i", "d"]
