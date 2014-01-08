module Elsantodel90.Geometry(Vector(..), FVector(..), Vector2D(..), (^)) where

-- TODO: Trabajar en la fixity de los operadores para que den cosas razonables.

import Prelude hiding ((+),(-),(*),(/),(^))
import qualified Prelude as P ((+),(-),(*),(/))

class Vector v where
    -- Hay que definir +
    (+) :: Num k => v k -> v k -> v k
    
    -- Hay que definir al menos uno de .-. y neg
    (-) :: Num k => v k -> v k -> v k
    v - w = v + neg w
    neg :: Num k => v k -> v k
    neg v = zero - v
    
    -- Hay que definir zero
    zero :: Num k => v k
    
    -- Hay que definir *
    (*) :: Num k => v k -> v k -> k
    
    -- Hay que definir al menos uno de *. y .*
    (.*) :: Num k => k -> v k -> v k
    (.*) = flip (*.)
    (*.) :: Num k => v k -> k -> v k
    (*.) = flip (.*)

class Vector v => FVector v where
    (/) :: Fractional k => v k -> k -> v k
    v / k = (1 P./ k) .* v

data Vector2D k = Vector2D k k deriving (Eq,Show)

(^) :: Num k => Vector2D k -> Vector2D k -> k
Vector2D x1 y1 ^ Vector2D x2 y2 = x1 P.* y2 P.- x2 P.* y1

instance Vector Vector2D where
    Vector2D x1 y1 + Vector2D x2 y2 = Vector2D (x1 P.+ x2) (y1 P.+ y2)
    Vector2D x1 y1 - Vector2D x2 y2 = Vector2D (x1 P.- x2) (y1 P.- y2)
    neg (Vector2D x y) = Vector2D (-x) (-y)
    zero = Vector2D 0 0
    Vector2D x1 y1 * Vector2D x2 y2 = x1 P.* x2 P.+ y1 P.* y2
    k .* Vector2D x y = Vector2D (k P.* x) (k P.* y)
    Vector2D x y *. k = Vector2D (k P.* x) (k P.* y)

instance FVector Vector2D where
    Vector2D x y / k = Vector2D (x P./ k) (y P./ k)
