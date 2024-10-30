module Tree where 

import qualified Data.Set as Set 

data SetTree a 
  = Node a (Set.Set (SetTree a))
  | Leaf a 


