module Main where

main = do undefined

-- Screen size is width=1080 height=1920
-- Bounded by the containing layout

type Size = (Int, Int)

data Orientation = Horizontal | Vertical

data View = LinearLayout Orientation [View]
          | TextView Size String

width :: View -> Int
width (LinearLayout Horizontal vs) = sum [width v | v <- vs]
width (LinearLayout Vertical vs) = maximum [width v | v <- vs]
width (TextView (w, _) _) = w

height :: View -> Int
height (LinearLayout Horizontal vs) = maximum [height v | v <- vs]
height (LinearLayout Vertical vs) = sum [height v | v <- vs]
height (TextView (_, h) _) = h

area :: View -> Int
area v = width v * height v

valid :: View -> Bool
valid (LinearLayout _ vs) = and [valid v | v <- vs]
valid (TextView (w, h) _) = w >= 0 && h >= 0
