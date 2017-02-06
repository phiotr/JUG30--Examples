import System.Environment (getArgs, getProgName)


main :: IO ()
main = do
    progName <- getProgName                         -- getProgName :: IO String
    progArgs <- getArgs                             -- getArgs :: IO [String]

    if (length progArgs) < 3 then
        putStrLn ("Usage: " ++  progName ++ " <width> <height> <rule>")
    else let
        width   = read $ progArgs !! 0
        height  = read $ progArgs !! 1
        rule    = read $ progArgs !! 2
      in
        printArguments width height rule


printArguments :: Int -> Int -> Int -> IO ()
printArguments w h r = do
    putStrLn "-- Comand line arguments --"
    putStrLn $ "-width\t: " ++ show w
    putStrLn $ "-height\t: " ++ show h
    putStrLn $ "-rule\t: " ++ show r
