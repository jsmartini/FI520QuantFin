#include <catch2/catch_test_macros.hpp>
#include "qlib.hpp"

using namespace QLib;

TEST_CASE("QLIB Test Suite", "[all]")
{

    SECTION("Bonds"){
        REQUIRE(bondF("2010-1-15", "2000-1-1", "2026-1-1", 2) == 0.14835164835164835);
        REQUIRE(bondNRemaining("2010-1-15", "2000-1-1", "2026-1-1", 2) == 32);
    }
}