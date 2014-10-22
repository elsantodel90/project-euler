module Elsantodel90.Pythagorean(primitives, triples, primitivesWithMaxX, triplesWithMaxX) where
import Elsantodel90.IntPot
import Prelude hiding ((^))

type Triple = (Integer,Integer,Integer)

triple :: Integer -> Integer -> Triple
triple x y = (x^2 - y^2, 2*x*y, x^2 + y^2)

multiples :: Triple -> [Triple]
multiples t@(a,b,c) = iterate (\(x,y,z) -> (x+a, y+b, z+c)) t

primitivesWithXIn :: [Integer] -> [Triple]
primitivesWithXIn l = [triple x y | x <- l, y <- [1..x-1], even x /= even y, gcd x y == 1]



-- All primitive triples (of the form x^2 - y^2 , 2*x*y, x^2 + y^2 ), by increasing x, then y
primitives :: [Triple]
primitives = primitivesWithXIn [1..]

primitivesWithMaxX :: Integer -> [Triple]
primitivesWithMaxX maxX = primitivesWithXIn [1..maxX]

-- All triples: For each primitive in primitives order, a list with all its associated triples, by increasing numbers
triples :: [[Triple]]
triples = map multiples primitives

triplesWithMaxX :: Integer -> [[Triple]]
triplesWithMaxX = map multiples . primitivesWithMaxX
