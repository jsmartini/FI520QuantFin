#pragma once

/*
    Main static header file to pull in other ones
    * to add a new section, simply create another folder with those headers and pull them in here to expose
        those files to the build system
*/

// bond related functions
#include "bonds/bonds.hpp"
#include "valuation/valuation.hpp"
#include "opt/optimization.hpp"
// expose common namespace
#define VERSION "0.0.1"

namespace QLib {
    
    void buildtest(){
        // demonstrates that the cmake build system is able to find this function and compile it into
        // and executable
        cout << "this is somehow working" << endl;
    };

    int build_test_fib(int i){return i == 0 ? 1 : i * build_test_fib(i-1);};
    void version(){cout << "FI520 QLib:\t" << VERSION << endl;};

};
