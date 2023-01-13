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

