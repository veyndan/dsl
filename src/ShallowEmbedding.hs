module ShallowEmbedding where

test = do
          print (design == ((200, 110), True, ["Title", "A long subtitle which wraps on to the next line", "Column A", "Column B"]))

design :: View
design = linearLayout Vertical [
             textView (200, 30) "Title",
             textView (170, 50) "A long subtitle which wraps on to the next line",
             linearLayout Horizontal [
                 textView (100, 20) "Column A",
                 textView (100, 30) "Column B"
             ]
         ]

type Size = (Int, Int)
data Orientation = Horizontal | Vertical

type View = (Size, Bool, [String])

linearLayout :: Orientation -> [View] -> View
linearLayout o vs =
    let
        ws        = [w | ((w, _), _, _) <- vs]
        hs        = [h | ((_, h), _, _) <- vs]
        valids    = [v | (_, v, _) <- vs]
        ts        = [t | (_, _, t) <- vs]
        size      = case o of Vertical   -> (maximum ws, sum hs)
                              Horizontal -> (sum ws, maximum hs)
        valid     = and valids
        text      = concat ts
    in (size, valid, text)

textView :: Size -> String -> View
textView s@(w, h) t = (s, w >= 0 && h >= 0, [t])
