#include <catch2/catch_test_macros.hpp>
#include "qlib.hpp"

using namespace QLib;

TEST_CASE("QLIB Test Suite", "[factorial]")
{
    REQUIRE( build_test_fib(1) == 1);
    REQUIRE( build_test_fib(2) == 2);
    REQUIRE( build_test_fib(3) == 6);
}