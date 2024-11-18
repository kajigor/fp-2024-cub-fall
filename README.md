# HW07 

## Soft deadline: 23:59 12.11.2024
## Hard deadline: 23:59 14.11.2024

1. [6 points] Implement property-based testing of your compiler and interpreter implemented in HW05.

2. [2 points] Show that the following lambda-terms are equivalent: 
    * `S K K == I`
    * `B == S (K S) K`
  
    S K K = (λxyz. x z (y z)) K K
    = (λyz. K z (y z)) K
    = (λz. K z (K z))
    K z = (λab. a) z = λb. z
    = (λz. (λb. z) (λb. z)) = (λz. z)
    S K K = I
    S (K S) K = (λxyz. x z (y z)) (K S) K
    K S = (λab. a) S = λb. S
    = (λyz. (λb. S) z (y z)) K
    = S
    = (λz. S (K z))
    K z = (λab. a) z = λb. z
    = (λz. S (λb. z))
    S (λb. z) = (λxyz. x z (y z)) (λb. z)
    = (λyz. (λb. z) z (y z)) = (λz. z (y z))
    = λx y z. x (y z)
    B = λxyz. x (y z)
    S (K S) K = B