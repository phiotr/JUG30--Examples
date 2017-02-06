module ModuleAutomata (generateCellularAutomata, toString) where

import Data.Bits (shiftR)

type Rule = Int
type Cell = Int
type CellularTrio = (Cell, Cell, Cell)
type CellularAutomataRow = [Cell]
type CellularAutomata = [CellularAutomataRow]


initialRow :: Int -> Cell -> CellularAutomataRow
initialRow width initial_cell = half_of_line ++ initial_cell : half_of_line
    where
        half_of_width = width `div` 2                   -- <=> div width 2
        half_of_line = take half_of_width $ repeat 0    -- take :: Int -> [a] -> [a]
                                                        -- repeat :: a -> [a]


nextRow :: Rule -> CellularAutomataRow -> CellularAutomataRow
nextRow rule line = 0 : converted_cells ++ [0]
    where
        converted_cells = map (transformTrio2Cell rule) (getTriplesFromRow line)


getTriplesFromRow :: CellularAutomataRow -> [CellularTrio]
getTriplesFromRow [] = []
getTriplesFromRow [c1, c2, c3] = [(c1, c2, c3)]
getTriplesFromRow (c1:c2:c3:rest) = (c1, c2, c3) : getTriplesFromRow (c2:c3:rest)
getTriplesFromRow _ = []


transformTrio2Cell :: Rule -> CellularTrio -> Cell
transformTrio2Cell rule (c1, c2, c3) = (rule `shiftR` decimal_value) `mod` 2
    where
        decimal_value = 4*c1 + 2*c2 + c3


generateRows :: Int -> Rule -> CellularAutomataRow -> CellularAutomata
generateRows 0 _ _ = []
generateRows h r prev = prev : generateRows (h-1) r next
    where
        next = nextRow r prev


generateCellularAutomata :: Int -> Int -> Rule -> CellularAutomata
generateCellularAutomata width height rule = generateRows height rule $ initialRow width 1


toString :: CellularAutomata -> String
toString = unlines . map show
