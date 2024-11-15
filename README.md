# HW07 

## Soft deadline: 23:59 12.11.2024
## Hard deadline: 23:59 14.11.2024

1. [6 points] Implement property-based testing of your compiler and interpreter implemented in HW05.
Answer: Interestingly, after writing the tests, it showed that my implementation is wrong. A test case that fails is:
```
let foo = 0 in 
    let bar = 1 in
        bar + 1
    foo + bar
```
Since there is no `StackMachine` instruction to remove the variable from the context, and instead of failing, this compiles.

2. [2 points] Show that the following lambda-terms are equivalent: 
    * `S K K == I`
    * `B == S (K S) K`

Answer:
```
S K K   = (\f g x -> f x (g x)) (\a b -> b) (\c d -> d) x =
        = (\g x -> (\a b -> b) x (g x)) (\c d -> d) x =
        = (\x -> (\a b -> b) x ((\c d -> d) x)) x =
        = (\a b -> b) x ((\c d -> d) x) =
        = (\b -> b) ((\c d -> d) x) =
        = (\b -> b) (\d -> d) =
        = (\d -> d) = I
```
```
S (K S) K f g x = (K S f) (K f) g x = S (K f) g x = (K f x) (g x) = f (g x) = B
```