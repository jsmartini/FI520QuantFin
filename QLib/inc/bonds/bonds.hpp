#pragma once

/*
    Bond Calculation Header File
*/

#include <iostream>
#include <boost/date_time.hpp>      // https://www.boost.org/doc/libs/1_31_0/libs/date_time/doc/class_date.html
#include <vector>
#include <algorithm>
#include <iterator>

namespace bt = boost::gregorian;    // use time library from boost 


using namespace std;

namespace QLib
{

        /*
            Inputs Needed (with examples based on data above):
            • Settle date (1/17/18)
            • Maturity date (11/15/27)
            • Coupon payment frequency per year (m = 2)
            • Annualized coupon rate in percent (C = 2.25, in decimal form in equations below)
            • Number of remaining coupon payments (N = 20)
            • Fraction of coupon payment period that has elapsed already (f = 63/181 = 0.348066, in
            decimal)
            • Annualized yield to maturity in percent (y = 2.535235, in decimal form in equations below)
            • Par value in dollars (Par = $1,000,000)
        
        */

    double bondF(string current_date, string settle_date, string maturity_date, unsigned int coupon_payment_frequency)
    {
        // convert strings into bt::date objects 
        // allows +- operations with sepcification for time resolution .days, .weeks, .years etc
        // assuming gregorian calendar
        // using From delimited date string where with order iso standard ordering: year-month-day (eg: 2002-1-25)
        bt::date current = bt::from_simple_string(current_date);
        bt::date settle = bt::from_simple_string(settle_date);
        bt::date maturity = bt::from_simple_string(maturity_date);
        vector<bt::date> payment_schedule;
        // find days between each payment
        double days_between_payment = (double)(365/coupon_payment_frequency);   // assuming 365 days in a year
        // initialize the array for loop comparison
        payment_schedule.push_back(settle + bt::days(days_between_payment));
        //generate payment schedule
        while ((payment_schedule.back() + bt::days(days_between_payment)) <= maturity) 
            payment_schedule.push_back(payment_schedule.back() + bt::days(days_between_payment));
            // POSSBILE ERROR :: returns the next highest idx. Literally same output as std::upper_bound
        vector<bt::date>::iterator closest_prior_payment = lower_bound(payment_schedule.begin(), payment_schedule.end(), current); // find closest date (lower bounded)
        double days_since_last_payment = (current - (*(closest_prior_payment-1))).days();
        return days_since_last_payment / days_between_payment;      // f = NAD/NTD
    };

    int bondNRemaining(string current_date, string settle_date, string maturity_date, unsigned int coupon_payment_frequency)
    {
        // convert strings into bt::date objects 
        // allows +- operations with sepcification for time resolution .days, .weeks, .years etc
        // assuming gregorian calendar
        // using From delimited date string where with order iso standard ordering: year-month-day (eg: 2002-1-25)
        bt::date current = bt::from_simple_string(current_date);
        bt::date settle = bt::from_simple_string(settle_date);
        bt::date maturity = bt::from_simple_string(maturity_date);
        vector<bt::date> payment_schedule;
        // find days between each payment
        double days_between_payment = (double)(365/coupon_payment_frequency);   // assuming 365 days in a year
        // initialize the array for loop comparison
        payment_schedule.push_back(settle + bt::days(days_between_payment));
        //generate payment schedule
        while ((payment_schedule.back() + bt::days(days_between_payment)) <= maturity) 
            payment_schedule.push_back(payment_schedule.back() + bt::days(days_between_payment));
        vector<bt::date>::iterator closest_prior_payment = upper_bound(payment_schedule.begin(), payment_schedule.end(), current); // find closest date (lower bounded)
        return std::distance(closest_prior_payment,payment_schedule.end());
    };

    /*
    double bondMarketValue(string current, string settle, string maturity, unsigned int coupon_payment_frequency, double coupon_rate);
    
    double bondAccruedInterest(string current, string settle, string maturity, unsigned int coupon_payment_frequency, double coupon_rate);

    double bondQuotePrice(string current, string settle, string maturity, unsigned int coupon_payment_frequency, double coupon_rate){
        return bondMarketValue(current,settle,maturity,coupon_payment_frequency,coupon_rate) - bondAccruedInterest(current,settle,maturity,coupon_payment_frequency,coupon_rate);
    };
    */
 
};