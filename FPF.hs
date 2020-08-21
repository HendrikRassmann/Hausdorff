--import Linear.V
--import Linear.V1
import Linear.V2
--import Linear.V3
import Linear.Vector
import Linear.Metric

import Data.Monoid
import Data.Foldable
import Data.List
import Control.Applicative

norm2D :: (Floating a) => (V2 a)-> a
norm2D = Linear.Metric.norm

metric2D :: (Floating a) => (V2 a) -> (V2 a) -> a
metric2D = Linear.Metric.distance

hausdorff :: (Floating a, Ord a) => [V2 a] -> [V2 a] -> a
hausdorff [] _ = 0
hausdorff _ [] = 0
hausdorff m1 m2 =
    let m1_to_m2 = maximum $ map (\p -> minimum (map (metric2D p) m2) ) m1
        m2_to_m1 = maximum $ map (\p -> minimum (map (metric2D p) m1) ) m2
    in max m1_to_m2 m2_to_m1
   
halfway :: (Floating a) => V2 a -> V2 a -> V2 a
halfway target start = (/2) <$> (liftA2 (+) target start)

-------------------------------------------------------

contractions :: (Floating a, Ord a) => [(V2 a) -> (V2 a)]
contractions = halfway <$> corners

corners :: (Floating a, Ord a) => [(V2 a)]
corners = [V2 0 0, V2 1 0, V2 0 1,V2 0.5 (sqrt 0.75)]

hds :: (Floating a, Ord a) => [a]
hds = zipWith hausdorff (iterate (contractions <*>) [V2 0 0]) (iterate (contractions <*>) [V2 0 1])

points :: (Floating a, Ord a) => [ [V2 a] ]
points = iterate ( nub.(contractions <*>) ) corners

points1corner = iterate ( nub.(contractions <*>) ) [V2 0 0]
