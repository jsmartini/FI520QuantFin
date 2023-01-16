#pragma once

/*
    Bond Calculations Class

    Assignment 1
    Assigned	Tasks
1. Develop generic intermediate bond functions that can handle the following tasks:
a. Solve for N (number of remaining cash flows in the bond)
b. Solve for f = NAD/NTD (fraction of coupon payment period that has elapsed already)
NAD = number of accrued days since last coupon payment
NTD = number of total days between last coupon payment and next coupon payment
(Ignore holiday and week-end effects such as “modified business following.”)
2. Develop generic bond functions that can calculate the following and demonstrate your
functions are consistent with the Bloomberg screen shot above.
a. Market Value of Bond ($979,228.25)
b. Accrued Interest ($3,915.75)
c. Quoted Bond Price ($975,312.50)
3. Develop a generic function to compute yield to maturity (on a semi-annual, bond equivalent
basis) and demonstrate your function is consistent with the Bloomberg screen shot above,
2.535235%)
4. Plot the relationship between yield to maturity (y = 0 to 20%) and the Market Value of Bond.
Overlay a zero coupon bond as well as a 4.5% coupon bond.
5. Plot the relationship between time to maturity (In years = Current to 0) and the Market Value of
Bond assuming the remaining parameters remain the same (e.g., yield to maturity). Be sure to
make your time steps small (e.g., less than one month). Overlay a zero coupon bond as well as
a 4.5% coupon bond.
6. Repeat #1 and #2 with the following three updated information sets:


*/

#include <Eigen/Dense>
#include "stats.hpp"
#include <iostream>
#include <boost/date_time.hpp>

namespace bt = boost::gregorian;

#include <map>  // expose output as dictionary-like object in python

using namespace std;

namespace QLib
{

    class Bond 
    {
        public: 

            // Bond Object Constructor
            Bond(
                string settle_date,
                string maturity_date,
                unsigned int m,
                double C,
                unsigned int N,
                double f,
                double y,
                double Par
            ) : m(m), C(C), N(N), f(f), y(y), Par(Par)  // directly assign this to the class values
            {
                // convert string denoted time into boost datetime objects for operations
                this->settle_date = bt::from_simple_string(settle_date);
                this->maturity_date = bt::from_simple_string(maturity_date);
            };
        

        
        private:
            bt::date settle_date;
            bt::date maturity_date;
            unsigned int m;
            double C;
            unsigned int N;
            double f;
            double y;
            double Par;
    };

};