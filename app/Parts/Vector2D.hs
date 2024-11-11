module Parts.Vector2D where


data Vector2D = Vector2D {
    x :: Float,
    y :: Float
} deriving Show

instance Num Vector2D where
    (+) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    (-) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
    (*) = error "Unimplemented"
    negate _ = error "Unimplemented"
    abs = error "Unimplemented"
    signum = error "Unimplemented"
    fromInteger = error "Unimplemented"
