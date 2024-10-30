module Tree where 

import qualified Data.Set as Set 

data SetTree a 
  = Node a (Set.Set (SetTree a))
  | Leaf a 
  deriving (Eq)


instance Ord a => Ord (SetTree a) where
  compare (Node _ _) (Leaf _) = LT
  compare (Leaf _) (Node _ _) = GT
  compare (Leaf x) (Leaf y) = compare x y
  compare (Node x xs) (Node y ys) =
    case compare x y of
      EQ -> compare xs ys
      other -> other
