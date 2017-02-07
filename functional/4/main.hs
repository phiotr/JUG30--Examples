import System.Environment (getArgs, getProgName)
import ModuleAutomata (generateCellularAutomata, toString)


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
        putStr . toString $ generateCellularAutomata (width-1) height rule
