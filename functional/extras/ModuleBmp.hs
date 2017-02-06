module ModuleBmp (saveBmp, rgbBmp) where

import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Bits


saveBmp :: String -> BL.ByteString -> IO ()
saveBmp path image = BL.writeFile path image


grayBmp :: [[Int]] -> BL.ByteString
grayBmp = generateBmp 1


rgbBmp :: [[Int]] -> BL.ByteString
rgbBmp = generateBmp 3


generateBmp :: Int -> [[Int]] -> BL.ByteString
generateBmp bytes_per_pixel pixel_array = BL.pack $ bmp_header ++ bmp_data
    where
        height = fromIntegral $ length pixel_array
        width = fromIntegral $ length (head pixel_array) `div` bytes_per_pixel
        data_padding = bmpDataPadding $ width * bytes_per_pixel
        bmp_header = bmpHeader bytes_per_pixel width height data_padding
        bmp_data = map fromIntegral $ concat $ map (appendPadding data_padding) pixel_array


appendPadding :: Int -> [Int] -> [Int]
appendPadding padding pixel_row =
    case padding of
        1 -> pixel_row ++ [0]
        2 -> pixel_row ++ [0,0]
        3 -> pixel_row ++ [0,0,0]
        _ -> pixel_row


-- Conversion of Integer to little-endian Bytes
leBytes :: Int -> [Word8]
leBytes int = [fromIntegral (int `shiftR` i) | i <- [0, 8, 16, 24]]


emptyWord :: [Word8]
emptyWord = [0, 0, 0, 0]


bmpDataPadding :: Int -> Int
bmpDataPadding bytes =
    case modulo of
    0 -> 0
    _ -> 4 - modulo
    where modulo = bytes `mod` 4


bmpHeader :: Int -> Int -> Int -> Int -> [Word8]
bmpHeader bytes_per_pixel width height data_padding =
    let
        header_size = 54
        data_size   = (bytes_per_pixel * width + data_padding) * height
        signature   = [66, 77]
        file_size   = leBytes $ header_size + data_size
        reserved    = emptyWord
        offset      = [fromIntegral header_size, 0, 0, 0]
        dib_header  = [40, 0, 0, 0]
        img_width   = leBytes width
        img_height  = leBytes (-height)
        cplanes     = [1, 0, fromIntegral $ 8 * bytes_per_pixel, 0]
        compression = emptyWord
        img_size    = leBytes data_size
        metrics     = [19, 11, 0, 0, 19, 11, 0, 0]
        palete      = [0, 0, 0, 0, 0, 0, 0, 0]
     in
        concat $ [      -- number of byte
            signature,  -- 1,2
            file_size,  -- 3,4,5,6
            reserved,   -- 7,8,9,10
            offset,     -- 11,12,13,14
            dib_header, -- 15,16,17,18
            img_width,  -- 19,20,21,22
            img_height, -- 23,24,25,26
            cplanes,    -- 27,28,29,30
            compression,-- 31,32,33,34
            img_size,   -- 35,36,37,38
            metrics,    -- 39,40,41,42,43,44,45,46
            palete]     -- 47,48,49,50,51,52,53,54







