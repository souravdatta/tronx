# tronx

A Clojure interpreter and runtime for programming language Tronx.

## Usage

```lisp

(proc factorial (n)
    (if (<= n 1)
        (return 1)
        (return (* n (factorial (- n 1))))))
        
(do
    (:= x 10)
    (writeln "factorial of " x " is " (factorial x)))

```

## License

Copyright © 2023 Sourav Datta (soura.jagat@gmail.com)

This program and the accompanying materials are made available under the
terms of the Apache License. See accompanying LICENSE file for more details.