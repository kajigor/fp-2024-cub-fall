# HW06
## Soft deadline: 23:59 05.11.2024
## Hard Deadline: 23:59 07.11.2024

Homework is in [monads/src/FailCont/](monads/src/FailCont/)

1. [1 point] Define type `FailCont` which encapsulates a computation with two possible continuations: one to call in case of a success, and the other to call in case of a failure. 
2. [3 points] Implement `Functor`, `Applicative` and `Monad` instances for `FailCont`.
3. [1 points] Implement interface functions `toFailCont` and `evalFailCont`. 
4. [1 points] Implement functions `addInts` and `divInts` which demonstrate the use of `FailCont`. 
5. [2 points] Implement another example of its use, and try to be creative. 

