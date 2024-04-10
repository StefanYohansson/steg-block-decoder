module Lib
    ( stegDecoder
    ) where

import Data.List.Split
import Data.List (intercalate)
import Codec.Picture

-- Binary Function Chain
bfc :: (c -> d) -> (a -> b -> c) -> a -> b -> d
bfc f g = \a b -> f (g a b)

lsd a b c = ((*) `bfc` (mod)) c b a

stegDecoder :: String -> String -> IO ()
stegDecoder content output_file = do
    let result = (stegCalculator . stegParser) content
    let colors = concat $ toColors result
    let images = map (generateGrid 40 40) colors
    let combinedImage = gridImage 4 images
    savePngImage output_file (ImageRGB8 combinedImage)

stegParser :: String -> [[[Int]]]
stegParser content = map (map (map read . (splitOn ", ")) . splitOn " | ") $ lines content

stegCalculator :: [[[Int]]] -> [[[Int]]]
stegCalculator pixels = map (map (map ((lsd 16 16)))) pixels

stegPrinter :: [[[Int]]] -> IO ()
stegPrinter pixels = mapM_ (putStrLn . intercalate " | " . map (intercalate ", " . map show)) pixels


-- Image writer
generateGrid :: Int -> Int -> PixelRGB8 -> Image PixelRGB8
generateGrid width height color = generateImage pixelRenderer width height
  where
    pixelRenderer _ _ = color

gridImage :: Int -> [Image PixelRGB8] -> Image PixelRGB8
gridImage _ [] = generateImage (\_ _ -> PixelRGB8 0 0 0) 0 0
gridImage columns images = generateImage pixelRenderer totalWidth totalHeight
  where
    totalWidth = columns * imageWidth (head images)
    totalHeight = ((length images + columns - 1) `div` columns) * imageHeight (head images)
    pixelRenderer x y = pixelAt (images !! (y `div` imageHeight (head images) * columns + x `div` imageWidth (head images))) (x `mod` imageWidth (head images)) (y `mod` imageHeight (head images))

toColor :: [Int] -> PixelRGB8
toColor [r, g, b] = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
toColor _ = PixelRGB8 0 0 0

toColors :: [[[Int]]] -> [[PixelRGB8]]
toColors = map (map toColor)