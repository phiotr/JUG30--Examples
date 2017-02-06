import System.Environment (getArgs, getProgName)


main :: IO ()
main = do
    progName <- getProgName                         -- getProgName :: IO String
    progArgs <- getArgs                             -- getArgs :: IO [String]

    if (length progArgs) < 3 then
        putStrLn ("Usage: " ++  progName ++ " <width> <height> <rule>")
    else let
        width   = read $ progArgs !! 0 :: Int
        height  = read $ progArgs !! 1 :: Int
        rule    = read $ progArgs !! 2 :: Int
      in
        putStrLn . show $ initialRow width 1


initialRow :: Int -> Int -> [Int]
initialRow width initial_cell = half_of_line ++ [initial_cell] ++ half_of_line
    where
        half_of_width = width `div` 2               -- <=> div width 2
        half_of_line = take half_of_width . repeat $ 0
                                                    -- take :: Int -> [a] -> [a]
                                                    -- repeat :: a -> [a]