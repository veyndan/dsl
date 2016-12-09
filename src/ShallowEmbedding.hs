module ShallowEmbedding where

test = do
          print (design == ((50, 65), True, ["Hello world", "It's Veyndan", "Wazaaaa", "The show has finished"]))

design :: View
design = linearLayout Vertical (0, 255, 0, 0.5) [
             textView (20, 30) (255, 0, 0, 0.3) "Hello world",
             textView (40, 15) (0, 0, 255, 1) "It's Veyndan",
             linearLayout Horizontal (127, 127, 0, 0.8) [
                 textView (10, 20) (0, 127, 127, 0.4) "Wazaaaa",
                 textView (40, 15) (0, 127, 0, 1) "The show has finished"
             ]
         ]

type Size = (Int, Int)
data Orientation = Horizontal | Vertical
type Background = (Int, Int, Int, Float)

type View = (Size, Bool, [String])

linearLayout :: Orientation -> Background -> [View] -> View
linearLayout Vertical _ vs = ((maximum [w | ((w, _), _, _) <- vs], sum [h | ((_, h), _, _) <- vs]), and [v | (_, v, _) <- vs], concat [t | (_, _, t) <- vs])
linearLayout Horizontal _ vs = ((sum [w | ((w, _), _, _) <- vs], maximum [h | ((_, h), _, _) <- vs]), and [v | (_, v, _) <- vs], concat [t | (_, _, t) <- vs])

textView :: Size -> Background -> String -> View
textView (w, h) _ t = ((w, h), w >= 0 && h >= 0, [t])
