module DeepEmbedding where

test = print (width design == 50 && height design == 65 && valid design
        && text design == ["Hello world", "It's Veyndan", "Wazaaaa", "The show has finished"])

design :: View
design = LinearLayout Vertical (0, 255, 0, 0.5) [
             TextView (20, 30) (255, 0, 0, 0.3) "Hello world",
             TextView (40, 15) (0, 0, 255, 1) "It's Veyndan",
             LinearLayout Horizontal (127, 127, 0, 0.8) [
                 TextView (10, 20) (0, 127, 127, 0.4) "Wazaaaa",
                 TextView (40, 15) (0, 127, 0, 1) "The show has finished"
             ]
         ]

type Size = (Int, Int)
data Orientation = Horizontal | Vertical
type Background = (Int, Int, Int, Float)

data View = LinearLayout Orientation Background [View]
          | TextView Size Background String

width :: View -> Int
width (LinearLayout Horizontal _ vs) = sum [width v | v <- vs]
width (LinearLayout Vertical _ vs) = maximum [width v | v <- vs]
width (TextView (w, _) _ _) = w

height :: View -> Int
height (LinearLayout Horizontal _ vs) = maximum [height v | v <- vs]
height (LinearLayout Vertical _ vs) = sum [height v | v <- vs]
height (TextView (_, h) _ _) = h

valid :: View -> Bool
valid (LinearLayout _ _ vs) = and [valid v | v <- vs]
valid (TextView (w, h) _ _) = w >= 0 && h >= 0

text :: View -> [String]
text (LinearLayout _ _ vs) = concat [text v | v <- vs]
text (TextView _ _ t) = [t]

-- https://en.wikipedia.org/wiki/Alpha_compositing
-- A LinearLayout can be nested in other LinearLayout's, so multiple render targets for alpha
-- compositing is necessary.
background :: View -> Background
background (LinearLayout _ (r, g, b, a) vs) = undefined
background (TextView _ (r, g, b, a) _) = undefined
