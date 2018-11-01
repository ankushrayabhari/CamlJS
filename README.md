# CamlJS

## Building and Running the Transpiler
Running ``` make ``` will generate an executable named ```camljs```. To compile a file, run:
```
./camljs [input source file] [output js file path]
```

## Running Tests
Running ``` make test ``` will run the unit test suite. The standard OCaml installation of all assignments is required.

## Running End to End Tests
Running ``` make e2e-test ``` will run the end to end test suite. 
Note that you must be on MacOS and have a recent version of NodeJS installed in order to execute the test suite.
