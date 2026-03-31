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
#include "ores.ore/domain/domain.hpp"

#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_exotic_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for Exotic
 * instruments: DoubleDigitalOption, EquityAccumulator, EquityCliquetOption,
 * EquityTaRF, EquityWorstOfBasketSwap, FXWorstOfBasketSwap,
 * Formula-Based Coupon (SwapData), FxAccumulator, FxGenericBarrierOption,
 * FxTaRF, KnockOutSwap, PerformanceOption (COM/FX), and RainbowOption.
 */

namespace {

const std::string_view test_suite("ores.ore.exotic.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][exotic]");

using ores::ore::domain::portfolio;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

std::filesystem::path golden_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "assets/test_data/golden_dataset/Products/Example_Trades/" + filename);
}

void run_golden_test(const std::string& filename) {
    using ores::platform::filesystem::file;
    auto lg(make_logger(test_suite));

    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    const std::string canonical = ores::ore::domain::save_data(p);

    const auto gpath = golden_path(filename);
    INFO("Golden file missing (run with --reset-goldens to bootstrap): " + filename);
    REQUIRE(std::filesystem::exists(gpath));

    const std::string golden = file::read_content(gpath);
    CHECK(canonical == golden);
    BOOST_LOG_SEV(lg, info) << "Golden roundtrip passed: " << filename;
}

} // namespace

// =============================================================================
// DoubleDigitalOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_double_digital_option", tags) {
    run_golden_test("Exotic_Double_Digital_Option.xml");
}

// =============================================================================
// EquityAccumulator tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_equity_accumulator_single_name", tags) {
    run_golden_test("Exotic_EquityAccumulator_single_name.xml");
}

// =============================================================================
// EquityCliquetOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_equity_cliquet_option", tags) {
    run_golden_test("Exotic_Equity_Cliquet_Option.xml");
}

// =============================================================================
// EquityTaRF tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_equity_tarf", tags) {
    run_golden_test("Exotic_EquityTaRF.xml");
}

// =============================================================================
// EquityWorstOfBasketSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_equity_worst_of_basket_swap", tags) {
    run_golden_test("Exotic_EquityWorstOfBasketSwap.xml");
}

TEST_CASE("golden_roundtrip_exotic_fx_worst_of_basket_swap", tags) {
    run_golden_test("Exotic_FXWorstOfBasketSwap.xml");
}

// =============================================================================
// Formula-Based Coupon (SwapData) tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_formula_based_coupon", tags) {
    run_golden_test("Exotic_Formula_Based_Coupon.xml");
}

// =============================================================================
// FxAccumulator tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_fx_accumulator", tags) {
    run_golden_test("Exotic_FxAccumulator.xml");
}

// =============================================================================
// FxGenericBarrierOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_fx_generic_barrier_option", tags) {
    run_golden_test("Exotic_FxGenericBarrierOption.xml");
}

// =============================================================================
// FxTaRF tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_fx_tarf", tags) {
    run_golden_test("Exotic_FxTaRF.xml");
}

// =============================================================================
// KnockOutSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_knock_out_swap", tags) {
    run_golden_test("Exotic_KnockOutSwap.xml");
}

// =============================================================================
// PerformanceOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_performance_option_com", tags) {
    run_golden_test("Exotic_PerformanceOption_01_COM.xml");
}

TEST_CASE("golden_roundtrip_exotic_performance_option_fx", tags) {
    run_golden_test("Exotic_PerformanceOption_01_FX.xml");
}

// =============================================================================
// RainbowOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_exotic_rainbow_option", tags) {
    run_golden_test("Exotic_RainbowOption.xml");
}
