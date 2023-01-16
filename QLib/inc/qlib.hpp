#pragma once

/*
    Main static header file to pull in other ones
    * to add a new section, simply create another folder with those headers and pull them in here to expose
        those files to the build system
*/

// bond related functions
#include "bonds/bonds.hpp"
// expose common namespace
namespace QLib {
    
    void buildtest(){
        // demonstrates that the cmake build system is able to find this function and compile it into
        // and executable

        cout << "this is somehow working" << endl;
    };

};
