# Lambda Calculus with additions 

Simple lambda-calculus language to illustrate how the CAOS library can be used to animate reduction steps.


## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to get the submodules dependencies (CAOS library), and later compile using ScalaJS.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `git submodule update --init`
2. `sbt fastLinkJS`
3. open the file `lib/tool/index.html`
