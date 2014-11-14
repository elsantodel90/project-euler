module Elsantodel90.IntPot ((^)) where

import Prelude hiding ((^))
import qualified Prelude ((^))

-- Permite mantener la notacion y evita warnings de GHC con -Wall, al avisarle explicitamente que usamos Int para exponentes.
(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

