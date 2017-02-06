module ModuleColorConverter (convert01ToMono, convert01ToGray) where


convert01ToMono :: [[Int]] -> [[Bool]]
convert01ToMono pixel_matrix = map (map (\cell -> cell == 1)) pixel_matrix


convert01ToGray :: [[Int]] -> [[Int]]
convert01ToGray pixel_matrix = map (map grayScale) pixel_matrix


grayScale :: Int -> Int
grayScale 1 = 0
grayScale _ = 255
