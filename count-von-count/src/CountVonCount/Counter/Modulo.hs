--------------------------------------------------------------------------------
module CountVonCount.Counter.Modulo
    ( Modulo
    , toModulo
    , fromModulo
    , (.+.)
    , (.-.)
    , (.*.)
    , (./.)
    ) where


--------------------------------------------------------------------------------
-- | Double-based modulo arithmetic
newtype Modulo = Modulo (Double -> Double)


--------------------------------------------------------------------------------
-- | Convert a double to a double in modulo arithmetic
toModulo :: Double -> Modulo
toModulo = Modulo . modulo


--------------------------------------------------------------------------------
-- | Convert a double in modulo arithmetic to a double in R. The infinite list
-- contains only positive possibilities and starts with the smallest value.
fromModulo :: Modulo -> Double -> [Double]
fromModulo (Modulo f) m = let x = f m in [x, x + m ..]


--------------------------------------------------------------------------------
-- | `mod` for doubles
modulo :: Double -> Double -> Double
modulo x m =
    let i = floor (x / m) :: Int
    in  x - m * fromIntegral i


--------------------------------------------------------------------------------
(.+.) :: Modulo -> Modulo -> Modulo
(.+.) = binop (+)


--------------------------------------------------------------------------------
(.-.) :: Modulo -> Modulo -> Modulo
(.-.) = binop (-)


--------------------------------------------------------------------------------
(.*.) :: Modulo -> Modulo -> Modulo
(.*.) = binop (*)


--------------------------------------------------------------------------------
(./.) :: Modulo -> Modulo -> Modulo
(./.) = binop (/)


--------------------------------------------------------------------------------
binop :: (Double -> Double -> Double) -> Modulo -> Modulo -> Modulo
binop (<>) (Modulo f) (Modulo g) = Modulo $ \m -> modulo (f m <> g m) m
