import Elsantodel90.Geometry hiding ((+),(*),(-),(/),(.*), (^))
import qualified Elsantodel90.Geometry as G ((+),(*),(-),(/),(.*))

type Point = Vector2D Double

norm p = sqrt $ p G.* p
normalize p = p G./ norm p

cuad a b c = [(-b + disc) / (2*a), (-b - disc) / (2*a)]
              where disc = sqrt $ b^2 - 4*a*c

maxAbs [a,b] = if abs a > abs b then a else b

reflect :: Point -> Point -> Point
reflect (Vector2D px py) v = v G.- ((2 * (v G.* tangent)) G..* tangent)
                 where tangent = normalize $ Vector2D  py  (-4 * px)
                       

step :: (Point, Point) -> (Point, Point)
step (p@(Vector2D px py),v@(Vector2D vx vy)) = (pNew, vNew)
                      where lambda = maxAbs $ cuad (4*vx^2 + vy^2) (8 * px * vx + 2 * py * vy) (4 * px^2 + py^2 - 100)
                            pNew   = p G.+ (lambda G..* v)
                            vNew   = reflect pNew v

impactPointIsInside (Vector2D x y ,_) = not $ -0.01 <= x && x <= 0.01 && y > 0.0

main = print . length  . takeWhile impactPointIsInside . tail $ iterate step (p0, v0)
         where p0 = Vector2D 0.0 10.1
               v0 = normalize $ Vector2D 1.4 (-9.6) G.- p0
