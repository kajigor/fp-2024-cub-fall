module Tree where 

import qualified Data.Set as Set 

data SetTree a 
  = Node a (Set.Set (SetTree a))
  | Leaf a 

instance Eq a => Eq (SetTree a) where
    (Leaf x) == (Leaf y) = x == y
    (Node x xs) == (Node y ys) = x == y && xs == ys
    _ == _ = False
    
instance Ord a => Ord (SetTree a) where
    (Leaf x) `compare` (Leaf y) = x `compare` y
    (Leaf x) `compare` (Node _ _) = LT
    (Node _ _) `compare` (Leaf _) = GT
    (Node x xs) `compare` (Node y ys) =
        case x `compare` y of
            EQ -> xs `compare` ys
            other -> other