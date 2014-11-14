import Elsantodel90.Geometry hiding ( (*), (-) )
import qualified Elsantodel90.Geometry as G

coords :: [Integer]
coords = [0..50]

(/<) :: Ord t => Vector2D t -> Vector2D t -> Bool
Vector2D x1 y1 /< Vector2D x2 y2 = (x1,y1) < (x2,y2)

rightTriangle :: (Vector2D Integer, Vector2D Integer) -> Bool
rightTriangle (v,w) = (v G.^ w) /= 0 && (v G.* w == 0 || (w G.- v) G.* v == 0 || (w G.- v) G.* w == 0 )

main :: IO ()
main = print . length $ filter rightTriangle [(v,w) |   x1 <- coords,
                                                        y1 <- coords, 
                                                        x2 <- coords, 
                                                        y2 <- coords,
                                                        let v = Vector2D x1 y1,
                                                        let w = Vector2D x2 y2,
                                                        v /< w ]
