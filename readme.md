# Lambda Calculus with additions 

Simple lambda-calculus language to illustrate how the CAOS library can be used to animate reduction steps, inspired by an example for PLT Redex's domain-specific language embedded in Racket for specifying and debugging operational semantics (https://redex.racket-lang.org/lam-v.html).

A snapshot of the compiled website can be found in the `docs` folder, and used online via the link below.

 - http://lmf.di.uminho.pt/lambda-caos/


## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to get the submodules dependencies (CAOS library), and later compile using ScalaJS.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `git submodule update --init`
2. `sbt fastLinkJS`
3. open the file `lib/tool/index.html`
