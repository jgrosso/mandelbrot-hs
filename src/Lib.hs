{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( plot
  ) where

import Codec.Picture
  ( DynamicImage(ImageRGB8)
  , PixelRGB8(PixelRGB8)
  , generateImage
  , savePngImage
  )

import Control.Arrow (second)
import Control.DeepSeq (NFData(rnf), force)
import Control.DeepSeq.Generics (genericRnf)
import Control.Exception (evaluate)
import Control.Monad.ST (runST)

import Data.Colour.SRGB (RGB(RGB), sRGB24read, toSRGB24)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Semigroup (Any, (<>))

import Debug.Trace (trace)

import GHC.Generics (Generic)

import Numeric (showHex)

import Text.Printf (printf)

deriving instance Generic PixelRGB8

instance NFData PixelRGB8 where
  rnf = genericRnf

data Coordinate =
  Coordinate !(Double, Double)

data Pixel =
  Pixel !(Double, Double)

screenHeight :: Int
screenHeight = 8094

screenWidth :: Int
screenWidth = 8094

screenPixels :: [Pixel]
screenPixels =
  curry Pixel <$> [0 .. fromIntegral screenWidth] <*>
  [0 .. fromIntegral screenHeight]

data Color =
  Color !Int

instance Show Color where
  show :: Color -> String
  show (Color color) = "#" <> (printf "%06s" $ showHex color "")

maxIterations :: Int
maxIterations = 5000

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

mandelbrot :: [[PixelRGB8]]
mandelbrot =
  map
    (\row ->
       map
         (\column ->
            let pixel = Pixel (row, column)
            in colorToRGB8 $ plotPixel pixel)
         [0 .. fromIntegral screenWidth])
    [0 .. fromIntegral screenHeight]

plot :: IO ()
plot = do
  mandelbrot' <- evaluate $ force mandelbrot
  putStrLn "Mandelbrot evaluated!"
  let generatePixel x y = mandelbrot' !! x !! y
  let image = ImageRGB8 $ generateImage generatePixel screenWidth screenHeight
  savePngImage "output.png" image

colorToRGB8 :: Color -> PixelRGB8
colorToRGB8 =
  (\(RGB red green blue) -> PixelRGB8 red green blue) .
  toSRGB24 . sRGB24read . show

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
      iteration =
        runST $ do
          coordRef <- newSTRef $ Coordinate (x0, y0)
          numIterationRef <- newSTRef 0
          whileM
            (do Coordinate (x, y) <- readSTRef coordRef
                numIteration <- readSTRef numIterationRef
                pure $ (x * x) + (y * y) < 4 && numIteration < maxIterations) $ do
            Coordinate (x, y) <- readSTRef coordRef
            let tempX = (x * x) - (y * y) + x0
            modifySTRef coordRef $
              (\(Coordinate (x, y)) -> Coordinate (tempX, (2 * x * y) + y0))
            modifySTRef numIterationRef succ
          readSTRef numIterationRef
  in iterationToColor iteration

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM cond f = do
  isIncomplete <- cond
  if isIncomplete
    then f >> whileM cond f
    else pure ()
