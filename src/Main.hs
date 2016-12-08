module Main where

main = do undefined

-- Screen size is width=1080 height=1920
-- Bounded by the containing layout

type Size = (Int, Int)

data View = VerticalLayout [View]
          | TextView Size String

width :: View -> Int
width (VerticalLayout vs) = maximum [width v | v <- vs]
width (TextView (w, _) _) = w

height :: View -> Int
height (VerticalLayout vs) = sum [height v | v <- vs]
height (TextView (_, h) _) = h

area :: View -> Int
area v = (width v) * (height v)

valid :: View -> Bool
valid (VerticalLayout vs) = and [valid v | v <- vs]
valid (TextView (w, h) text) = w >= 0 && h >= 0
