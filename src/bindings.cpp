/*
    Bindings file

    Add bindings definitions for everything in the QLib namespace

*/


#include <pybind11/pybind11.h>
#include "qlib.hpp"

namespace py = pybind11;

// bind C++  to python3 here
PYBIND11_MODULE(QLib, m)
{
    // test bindings
    m.def(
        "ver",
        &QLib::version,
        "version function"
    );
};