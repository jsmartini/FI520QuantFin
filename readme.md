This is the repository for FI520 class for quant finance

Project Structure
- Dep
  - eigen (eigen 3 linalg library)
  - gcem (statistic library required by stats library)
  - stats (static header library that is interoperable with eigen3)
  - googletest (test library)

- QLib
  - static library for defining functions for class assignments
  - requires everything in Dep

- Main
  - Assignment main files
    - append a new add_executable to the top-level cmake file to add assignments to build

### Main Application Code
pybind11 module code:
src/bindings.cpp

CMakeLists.txt and setup.py taken from -> https://github.com/pybind/cmake_example

1/15/23 - python3 bindings are working!

>import QLib
>QLib.ver()
FI520 QLib:     0.0.1
 


This file needs to be in the same directory python3 is invoked!
build/QLib.cpython-310-x86_64-linux-gnu.so

### Unit test cases
- tests/... blah blah blah