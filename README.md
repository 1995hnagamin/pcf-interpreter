PCF Interpreter
===============

# How to build
Requirements:
* OCaml
* ocamlfind
* ocamlyacc/ocamllex
* OMake

    git clone https://github.com/1995hnagamin/pcf-interpreter.git
    cd pcf-interpreter
    omake

# Example

    $ ./pcf.opt
    # succ(succ(100))
    102
    # iszero(1)
    false
    # if true then 100 else 200
    100
    # ((lambda x:num.iszero(pred(x))) 1)
    true
