import System.Environment (getArgs, getProgName)
import ModuleAutomata
import ModuleResize


main :: IO ()
main = do
    progName <- getProgName                         -- getProgName :: IO String
    progArgs <- getArgs                             -- getArgs :: IO [String]

    if (length progArgs) < 4 then
        putStrLn ("Usage: " ++  progName ++ " <width> <height> <rule> <zoom>")
    else let
        width   = read $ progArgs !! 0 :: Int
        height  = read $ progArgs !! 1 :: Int
        rule    = read $ progArgs !! 2 :: Int
        zoom    = read $ progArgs !! 3 :: Int

        ca_width = (width-1) `div` zoom
        ca_height = height `div` zoom

        output  = "fractals/hs-" ++ show rule ++ "-" ++ show zoom ++ "-" ++ show width ++ "x" ++ show height ++ ".txt"
      in
         writeFile output . toPrettyString . resizeXY zoom $ generateCellularAutomata ca_width ca_height rule
