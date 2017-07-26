## This is some Haskell code that implement SICP example

The following is  SICP note

## preface

* programs must be written for people to read and incidentally for machines to execute
* the computer revolution is a revolution in the way we think and in the way we express what we think

## ch1: Building Abstractions with procedures
* powerful languages: more than just instructing a computer to perform tasks, also serves as a framework within which we organize  our ideas about process
* normal order Or applicative order evaluation (lazy)
* recursive process VS iterative process
    - recursive process: keep track of the operations to be performed later on
   - iterative process: only keep track of fixed number of sate variables
* recursive procedure , recursive process
    - recursive procedure means syntacitc fact that refer to itself
    -  recursive process is how the process evolves
* tail recursive: it will implement recursive procedure in iterative process ( constant space)
* As programmer, we should identify the underlying abstractions in our programs and choose the proper level of abstractioni
