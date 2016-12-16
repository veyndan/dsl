module DeepEmbedding where

test = print (width design == 50 && height design == 65 && valid design
        && text design == ["Hello world", "It's Veyndan", "Wazaaaa", "The show has finished"])

design :: View
design = LinearLayout Vertical [
             TextView (20, 30) "Hello world",
             TextView (40, 15) "It's Veyndan",
             LinearLayout Horizontal [
                 TextView (10, 20) "Wazaaaa",
                 TextView (40, 15) "The show has finished"
             ]
         ]

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

valid :: View -> Bool
valid (LinearLayout _ vs) = and [valid v | v <- vs]
valid (TextView (w, h) _) = w >= 0 && h >= 0

text :: View -> [String]
text (LinearLayout _ vs) = concat [text v | v <- vs]
text (TextView _ t) = [t]
