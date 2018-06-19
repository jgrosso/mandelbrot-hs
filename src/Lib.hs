{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( plot
  ) where

import Control.Arrow (second)
import Control.Monad.State (execState, get, gets, modify, put)

import Data.Colour.SRGB (sRGB24read)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Semigroup (Any, (<>))

import Debug.Trace (traceId)

import Diagrams.Attributes as Svg (lineWidth, none)
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Core.Types as Svg (QDiagram)
import Diagrams.Size as Svg (mkSizeSpec)
import Diagrams.TwoD.Attributes as Svg (fillTexture, solid)
import Diagrams.TwoD.Combinators as Svg (hcat, vcat)
import Diagrams.TwoD.Shapes as Svg (square)
import Diagrams.TwoD.Types as Svg (V2(V2))

import Numeric (showHex)

import Text.Printf (printf)

type SvgDiagram = Svg.QDiagram SVG Svg.V2 Double Any

newtype Coordinate =
  Coordinate (Double, Double)

newtype Pixel =
  Pixel (Double, Double)

screenHeight :: Int
screenHeight = 1024

screenWidth :: Int
screenWidth = 1024

screenPixels :: [Pixel]
screenPixels =
  curry Pixel <$> [0 .. fromIntegral screenWidth] <*>
  [0 .. fromIntegral screenHeight]

newtype Color =
  Color Int

instance Show Color where
  show :: Color -> String
  show (Color color) = "#" <> (printf "%06s" $ showHex color "")

maxIterations :: Int
maxIterations = 10000

iterationToColor :: Int -> Color
iterationToColor =
  Color .
  round .
  scale (Interval 0 (fromIntegral paletteSize)) (Interval 0x000000 0xFFFFFF) .
  fromIntegral . (`mod` paletteSize)
  where
    paletteSize :: Int
    paletteSize = maxIterations `div` 80

dup :: a -> (a, a)
dup x = (x, x)

data Interval = Interval
  { lowerBound :: Double
  , upperBound :: Double
  }

mandelbrot :: [[Pixel]]
mandelbrot =
  map
    (\row ->
       map
         (\column ->
            let pixel = Pixel (fromIntegral row, fromIntegral column)
                color = plotPixel pixel
            in colorToSvgRect color)
         [0 .. screenWidth])
    [0 .. screenHeight]

plot :: IO ()
plot = savePngImage "output.png" image
  where
    image = ImageRGB8 $ generateImage generatePixel screenWidth screenHeight
    generatePixel x y = mandelbrot !! x !! y

colorToSvgRect color =
  let rgbColor = sRGB24read $ show color
      pointWidth = 1
  in Svg.square pointWidth & Svg.lineWidth Svg.none &
     Svg.fillTexture (Svg.solid rgbColor)

scale :: Interval -> Interval -> Double -> Double
scale input output x =
  let inputRange = upperBound input - lowerBound input
      outputRange = upperBound output - lowerBound output
  in ((x / inputRange) * outputRange) + lowerBound output

-- TODO Use `lens`
plotPixel :: Pixel -> Color
plotPixel pixel@(Pixel (pixelX, pixelY)) =
  let x0 =
        scale (Interval 0 (fromIntegral screenWidth)) (Interval (-2.5) 1) pixelX
      y0 =
        scale (Interval 0 (fromIntegral screenWidth)) (Interval (-1) 1) pixelY
      (_, iteration) =
        flip execState ((x0, y0), 0) $
        whileM
          (gets $ \((x, y), iteration) ->
             (x * x) + (y * y) < 4 && iteration < maxIterations) $ do
          tempX <- gets $ \((x, y), _) -> (x * x) - (y * y) + x0
          modify $ \((x, y), iteration) ->
            ((tempX, (2 * x * y) + y0), iteration + 1)
  in iterationToColor iteration

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM cond f = do
  isIncomplete <- cond
  if isIncomplete
    then f >> whileM cond f
    else pure ()
