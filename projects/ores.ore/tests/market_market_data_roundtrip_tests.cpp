/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore/market/market_data_parser.hpp"
#include "ores.ore/market/market_data_serializer.hpp"

#include <sstream>
#include <string>
#include <vector>
#include <catch2/catch_test_macros.hpp>
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file market_market_data_roundtrip_tests.cpp
 * @brief Roundtrip coverage for all 52 unique ORE market data text files.
 *
 * Strategy: parse → serialize → re-parse, then assert structural equality
 * (entry count and each date/key/value triple).  Comparison is field-by-field
 * rather than byte-identical because comment lines are dropped on serialization
 * and column spacing may differ.
 */

namespace {

const std::string tags("[ore][market][roundtrip]");

std::filesystem::path examples_path(const std::string& rel) {
    return ores::testing::project_root::resolve("external/ore/examples/" + rel);
}

void check_market_roundtrip(const std::string& rel_path) {
    using ores::platform::filesystem::file;
    using ores::ore::market::parse_market_data;
    using ores::ore::market::serialize_market_data;

    const auto path = examples_path(rel_path);
    INFO("fixture: " << rel_path);
    REQUIRE(std::filesystem::exists(path));

    const std::string content = file::read_content(path);

    // First parse
    std::istringstream in1{content};
    const auto original = parse_market_data(in1);

    // Serialize
    std::ostringstream out;
    serialize_market_data(out, original);

    // Re-parse
    std::istringstream in2{out.str()};
    const auto roundtripped = parse_market_data(in2);

    REQUIRE(roundtripped.size() == original.size());
    for (std::size_t i = 0; i < original.size(); ++i) {
        INFO("entry " << i);
        CHECK(roundtripped[i].date  == original[i].date);
        CHECK(roundtripped[i].key   == original[i].key);
        CHECK(roundtripped[i].value == original[i].value);
    }
}

void check_fixings_roundtrip(const std::string& rel_path) {
    using ores::platform::filesystem::file;
    using ores::ore::market::parse_fixings;
    using ores::ore::market::serialize_fixings;

    const auto path = examples_path(rel_path);
    INFO("fixture: " << rel_path);
    REQUIRE(std::filesystem::exists(path));

    const std::string content = file::read_content(path);

    std::istringstream in1{content};
    const auto original = parse_fixings(in1);

    std::ostringstream out;
    serialize_fixings(out, original);

    std::istringstream in2{out.str()};
    const auto roundtripped = parse_fixings(in2);

    REQUIRE(roundtripped.size() == original.size());
    for (std::size_t i = 0; i < original.size(); ++i) {
        INFO("entry " << i);
        CHECK(roundtripped[i].date       == original[i].date);
        CHECK(roundtripped[i].index_name == original[i].index_name);
        CHECK(roundtripped[i].value      == original[i].value);
    }
}

} // namespace

// =============================================================================
// Market data files — YYYYMMDD date format (17 files)
// =============================================================================

TEST_CASE("roundtrip_market_Input_market_20160205", tags) {
    check_market_roundtrip("Input/market_20160205.txt");
}
TEST_CASE("roundtrip_market_Input_market_20160205_flat", tags) {
    check_market_roundtrip("Input/market_20160205_flat.txt");
}
TEST_CASE("roundtrip_market_CurveBuilding_Input_market_20160205", tags) {
    check_market_roundtrip("CurveBuilding/Input/market_20160205.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_19_market_20160205_smile", tags) {
    check_market_roundtrip("Legacy/Example_19/Input/market_20160205_smile.txt");
}
TEST_CASE("roundtrip_market_AmericanMonteCarlo_Input_market_flat_fixed_fxfwd", tags) {
    check_market_roundtrip("AmericanMonteCarlo/Input/market_20160205_flat_fixed_fxfwd.txt");
}
TEST_CASE("roundtrip_market_Exposure_Input_market_flipview", tags) {
    check_market_roundtrip("Exposure/Input/market_flipview.txt");
}
TEST_CASE("roundtrip_market_InitialMargin_Input_Simm_simm_market", tags) {
    check_market_roundtrip("InitialMargin/Input/Simm/simm_market.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_56_market", tags) {
    check_market_roundtrip("Legacy/Example_56/Input/market.txt");
}
TEST_CASE("roundtrip_market_Academy_TA002_IR_Swap_market", tags) {
    check_market_roundtrip("Academy/TA002_IR_Swap/Input/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_2_market", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_2/Input/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_3_market", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_3/Input/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_6_market", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_6/Input/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_7_market", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_7/Input/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_9_market_20160205", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_9/Input/market_20160205.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_9_market_20160205_flat", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_9/Input/market_20160205_flat.txt");
}
TEST_CASE("roundtrip_market_XvaRisk_market_20160205_eonia_200bp_up", tags) {
    check_market_roundtrip("XvaRisk/Input/market_20160205_eonia_200bp_up.txt");
}
TEST_CASE("roundtrip_market_XvaRisk_market_20160205_eur6m_200bp_up", tags) {
    check_market_roundtrip("XvaRisk/Input/market_20160205_eur6m_200bp_up.txt");
}

// =============================================================================
// Market data files — YYYY-MM-DD date format (15 files)
// =============================================================================

TEST_CASE("roundtrip_market_AmericanMonteCarlo_Input_market", tags) {
    check_market_roundtrip("AmericanMonteCarlo/Input/market.txt");
}
TEST_CASE("roundtrip_market_Academy_FC003_Reporting_Currency_marketdata", tags) {
    check_market_roundtrip("Academy/FC003_Reporting_Currency/Input/marketdata.txt");
}
TEST_CASE("roundtrip_market_Academy_TA001_Equity_Option_marketdata", tags) {
    check_market_roundtrip("Academy/TA001_Equity_Option/Input/marketdata.txt");
}
TEST_CASE("roundtrip_market_Exposure_Input_market_inflation", tags) {
    check_market_roundtrip("Exposure/Input/market_inflation.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_45_market", tags) {
    check_market_roundtrip("Legacy/Example_45/Input/market.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_46_market", tags) {
    check_market_roundtrip("Legacy/Example_46/Input/market.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_47_market", tags) {
    check_market_roundtrip("Legacy/Example_47/Input/market.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_48_market", tags) {
    check_market_roundtrip("Legacy/Example_48/Input/market.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_55_market", tags) {
    check_market_roundtrip("Legacy/Example_55/Input/market.txt");
}
TEST_CASE("roundtrip_market_Legacy_Example_73_market", tags) {
    check_market_roundtrip("Legacy/Example_73/Input/market.txt");
}
TEST_CASE("roundtrip_market_MarketRisk_Input_Curvealgebra_market", tags) {
    check_market_roundtrip("MarketRisk/Input/Curvealgebra/market.txt");
}
TEST_CASE("roundtrip_market_MarketRisk_Input_HistSimVar_market", tags) {
    check_market_roundtrip("MarketRisk/Input/HistSimVar/market.txt");
}
TEST_CASE("roundtrip_market_MarketRisk_Input_Pnl_market", tags) {
    check_market_roundtrip("MarketRisk/Input/Pnl/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_5_market", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_5/Input/market.txt");
}
TEST_CASE("roundtrip_market_OREPython_Example_8_Example_62_market", tags) {
    check_market_roundtrip("ORE-Python/Notebooks/Example_8/Input/Example_62/market.txt");
}

// =============================================================================
// Fixings files — YYYYMMDD date format (2 files)
// =============================================================================

TEST_CASE("roundtrip_fixings_Legacy_Example_54_fixings_20160205", tags) {
    check_fixings_roundtrip("Legacy/Example_54/Input/fixings_20160205.txt");
}
TEST_CASE("roundtrip_fixings_OREPython_Example_9_fixings_20160205", tags) {
    check_fixings_roundtrip("ORE-Python/Notebooks/Example_9/Input/fixings_20160205.txt");
}

// =============================================================================
// Fixings files — YYYY-MM-DD date format (12 files)
// =============================================================================

TEST_CASE("roundtrip_fixings_Input_fixings_20160205", tags) {
    check_fixings_roundtrip("Input/fixings_20160205.txt");
}
TEST_CASE("roundtrip_fixings_AmericanMonteCarlo_Input_fixings", tags) {
    check_fixings_roundtrip("AmericanMonteCarlo/Input/fixings.txt");
}
TEST_CASE("roundtrip_fixings_Exposure_Input_fixings_inflation", tags) {
    check_fixings_roundtrip("Exposure/Input/fixings_inflation.txt");
}
TEST_CASE("roundtrip_fixings_Legacy_Example_45_fixings", tags) {
    check_fixings_roundtrip("Legacy/Example_45/Input/fixings.txt");
}
TEST_CASE("roundtrip_fixings_Legacy_Example_46_fixings", tags) {
    check_fixings_roundtrip("Legacy/Example_46/Input/fixings.txt");
}
TEST_CASE("roundtrip_fixings_Legacy_Example_47_fixings", tags) {
    check_fixings_roundtrip("Legacy/Example_47/Input/fixings.txt");
}
TEST_CASE("roundtrip_fixings_Legacy_Example_48_fixings", tags) {
    check_fixings_roundtrip("Legacy/Example_48/Input/fixings.txt");
}
TEST_CASE("roundtrip_fixings_MarketRisk_Input_HistSimVar_fixings", tags) {
    check_fixings_roundtrip("MarketRisk/Input/HistSimVar/fixings.txt");
}
TEST_CASE("roundtrip_fixings_MarketRisk_Input_Pnl_fixings", tags) {
    check_fixings_roundtrip("MarketRisk/Input/Pnl/fixings.txt");
}
TEST_CASE("roundtrip_fixings_OREAPI_Input_fixings_20160205", tags) {
    check_fixings_roundtrip("ORE-API/Input/fixings_20160205.txt");
}
TEST_CASE("roundtrip_fixings_OREPython_Example_3_fixings", tags) {
    check_fixings_roundtrip("ORE-Python/Notebooks/Example_3/Input/fixings.txt");
}
TEST_CASE("roundtrip_fixings_OREPython_Example_5_fixings", tags) {
    check_fixings_roundtrip("ORE-Python/Notebooks/Example_5/Input/fixings.txt");
}
