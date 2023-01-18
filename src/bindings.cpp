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
    m.def("ver",&QLib::version,"version function");

    /*
        Bonds Python3 Submodule
    */
    py::module bonds = m.def_submodule("Bonds", "Bonds Library");
    bonds.def("bondF",&QLib::Bonds::bondF,"f = NAD/NTD (fraction of coupon payment period that has elapsed already)");
    bonds.def("bondNRemaining", &QLib::Bonds::bondNRemaining, "number of periods remaining <int>");

    /*
        Optimization Python3 Submodule
    */
   py::module opt = m.def_submodule("Opt", "Convex Optimization Functions");


   /*
        Instruments Python3 Submodule
   */
    py::module Instruments = m.def_submodule("Instruments", "Financial Instruments Functions");


    /*
            Simulation Python3 Submodule
    */

    py::module Sim = m.def_submodule("Simulation", "Monte Carlo and Brownian Motion");


    /*
            Valuation Python3 Submodule
    */

    py::module Valuation = m.def_submodule("Valuation", "Valuation Functions");


};