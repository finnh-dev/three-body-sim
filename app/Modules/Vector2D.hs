-- | Module: Modules.Vector2D
-- This module defines a 2D vector type and implements basic vector operations.

module Modules.Vector2D where

-- | A 2D vector with `x` and `y` components.
data Vector2D = Vector2D {
    x :: Float, -- ^ The x-coordinate of the vector.
    y :: Float  -- ^ The y-coordinate of the vector.
} deriving Show

-- | `Num` instance for `Vector2D`, implementing addition and subtraction.
-- Multiplication, negation, and other methods are intentionally left unimplemented to avoid
-- misuse, as these operations don't have a direct meaning for vectors without additional context.

instance Num Vector2D where
    -- | Adds two vectors component-wise.
    (+) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    
    -- | Subtracts two vectors component-wise.
    (-) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
    
    -- | Multiplication is not defined for vectors. Throws an error if attempted.
    (*) = error "Unimplemented"

    -- | Negation is not implemented for vectors. Throws an error if attempted.
    negate _ = error "Unimplemented"

    -- | Absolute value is not defined for vectors. Throws an error if attempted.
    abs = error "Unimplemented"

    -- | Signum is not defined for vectors. Throws an error if attempted.
    signum = error "Unimplemented"

    -- | Conversion from an integer to a vector is not defined. Throws an error if attempted.
    fromInteger = error "Unimplemented"
