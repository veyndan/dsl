module DeepEmbedding where

test = print (width design == 200 && height design == 110 && valid design
        && text design == ["Title", "A long subtitle which wraps on to the next line", "Column A", "Column B"])

design :: View
design = LinearLayout Vertical [
             TextView (200, 30) "Title",
             TextView (170, 50) "A long subtitle which wraps on to the next line",
             LinearLayout Horizontal [
                 TextView (100, 20) "Column A",
                 TextView (100, 30) "Column B"
             ]
         ]

type Width = Int
type Height = Int
type Size = (Width, Height)
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

valid :: View -> Bool
valid (LinearLayout _ vs) = and [valid v | v <- vs]
valid (TextView (w, h) _) = w >= 0 && h >= 0

text :: View -> [String]
text (LinearLayout _ vs) = concat [text v | v <- vs]
text (TextView _ t) = [t]

optimized :: View -> Bool
optimized (LinearLayout _ vs) = length vs > 1 && and [optimized v | v <- vs]
optimized (TextView _ _) = True
