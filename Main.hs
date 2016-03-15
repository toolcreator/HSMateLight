import MateLight
import ListFrame

import Data.Maybe

testframe1 :: ListFrame
testframe1 = ListFrame $ concat $ replicate 8 [black, white]
  where
  white = replicate 40 (Pixel 0xff 0xff 0xff)
  black = replicate 40 (Pixel 0 0 0)
testframe2 :: ListFrame
testframe2 = ListFrame $ concat $ replicate 8 [white, black]
  where
  white = replicate 40 (Pixel 0x0 0xff 0xff)
  black = replicate 40 (Pixel 0 0 0)

testfkt :: (Int, Int) -> [Event] -> Bool -> (ListFrame, Bool)
testfkt _dim _evs s | s = (testframe1, not s)
                    | otherwise = (testframe2, not s)

move :: (Int, Int) -> String -> (Int, Int) -> (Int, Int)
move (xdim, ydim) "j" (x, y) = (x, (y + 1) `mod` ydim)
move (xdim, ydim) "k" (x, y) = (x, (y - 1) `mod` ydim)
move (xdim, ydim) "h" (x, y) = ((x - 1) `mod` xdim, y)
move (xdim, ydim) "l" (x, y) = ((x + 1) `mod` xdim, y)
move _ _ x = x

toFrame :: (Int, Int) -> (Int, Int) -> ListFrame
toFrame (xdim, ydim) (x', y') = ListFrame $ map (\y -> map (\x -> if x == x' && y == y' then Pixel 0xff 0xff 0xff else Pixel 0 0 0) [0 .. xdim - 1]) [0 .. ydim - 1]

eventTest :: (Int, Int) -> [Event] -> (Int, Int) -> (ListFrame, (Int, Int))
eventTest dim events pixel = (toFrame dim pixel', pixel')
  where pixel' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) pixel events

main :: IO ()
main = do
  runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 (40, 16) (Just 500000) True []) eventTest (0, 0)
