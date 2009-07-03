import Data.List

-- main = mapM_ putStr $ map ((++ "# --\n") . unlines)
--        $ permutations ["  a", "  a", "  + i", "  * i", "0000-00-00"]

main = putStrLn $ concatMap (\(x,y) -> x++y) (xs `zip` repeat "-")
    where
      xs = concat $ permutations ["a", "a", "i", "i", "d"]
