module ShallowEmbedding where

test = do
          print (design == ((50, 65), True, ["Hello world", "It's Veyndan", "Wazaaaa", "The show has finished"]))

design :: View
design = linearLayout Vertical [
             textView (20, 30) "Hello world",
             textView (40, 15) "It's Veyndan",
             linearLayout Horizontal [
                 textView (10, 20) "Wazaaaa",
                 textView (40, 15) "The show has finished"
             ]
         ]

type Size = (Int, Int)
data Orientation = Horizontal | Vertical

type View = (Size, Bool, [String])

linearLayout :: Orientation -> [View] -> View
linearLayout Vertical vs = ((maximum [w | ((w, _), _, _) <- vs], sum [h | ((_, h), _, _) <- vs]), and [v | (_, v, _) <- vs], concat [t | (_, _, t) <- vs])
linearLayout Horizontal vs = ((sum [w | ((w, _), _, _) <- vs], maximum [h | ((_, h), _, _) <- vs]), and [v | (_, v, _) <- vs], concat [t | (_, _, t) <- vs])

textView :: Size -> String -> View
textView s@(w, h) t = (s, w >= 0 && h >= 0, [t])
