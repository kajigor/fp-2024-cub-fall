module Tree where 

import qualified Data.Set as Set 

data SetTree a 
  = Node a (Set.Set (SetTree a))
  | Leaf a 
  deriving (Eq, Show)

instance Ord a => Ord (SetTree a) where
    compare (Leaf x) (Leaf y) = compare x y
    compare (Leaf _) (Node _ _) = LT
    compare (Node _ _) (Leaf _) = GT
    compare (Node x xs) (Node y ys) =
        case compare x y of
            EQ -> compare xs ys
            result -> result