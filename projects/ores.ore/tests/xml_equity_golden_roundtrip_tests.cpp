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

#include <fstream>
#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_equity_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for Equity
 * instruments: EquityOption, EquityForward, EquitySwap, EquityVarianceSwap,
 * barrier/touch/digital variants, EquityAsianOption, and
 * EquityOutperformanceOption.
 *
 * The margin trade TRS uses SwapData (already dispatched by the swap mapper).
 */

namespace {

const std::string_view test_suite("ores.ore.equity.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][equity]");

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
    if (!std::filesystem::exists(gpath)) {
        std::filesystem::create_directories(gpath.parent_path());
        file::write_content(gpath, canonical);
        SUCCEED("Bootstrapped golden file: " + filename);
        return;
    }

    const std::string golden = file::read_content(gpath);
    CHECK(canonical == golden);
    BOOST_LOG_SEV(lg, info) << "Golden roundtrip passed: " << filename;
}

} // namespace

// =============================================================================
// EquityOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_option_european", tags) {
    run_golden_test("Equity_Option_European.xml");
}

TEST_CASE("golden_roundtrip_equity_option_european_isin_ccy_mic", tags) {
    run_golden_test("Equity_Option_European_ISIN_CCY_MIC.xml");
}

// =============================================================================
// EquityForward tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_forward", tags) {
    run_golden_test("Equity_Forward.xml");
}

// =============================================================================
// EquitySwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_swap", tags) {
    run_golden_test("Equity_Swap.xml");
}

TEST_CASE("golden_roundtrip_equity_swap_indexed_funding_leg", tags) {
    run_golden_test("Equity_Swap_Indexed_Funding_Leg.xml");
}

TEST_CASE("golden_roundtrip_equity_margin_trade_trs", tags) {
    run_golden_test("Equity_Margin_Trade_TRS_on_EquityIndexFuture.xml");
}

// =============================================================================
// EquityVarianceSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_variance_swap", tags) {
    run_golden_test("Equity_Variance_Swap.xml");
}

// =============================================================================
// EquityBarrierOption / EquityTouchOption / EquityDigitalOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_barrier_option", tags) {
    run_golden_test("Equity_Barrier_Option.xml");
}

TEST_CASE("golden_roundtrip_equity_double_barrier_option", tags) {
    run_golden_test("Equity_Double_Barrier_Option.xml");
}

TEST_CASE("golden_roundtrip_equity_european_barrier_option", tags) {
    run_golden_test("Equity_European_Barrier_Option.xml");
}

TEST_CASE("golden_roundtrip_equity_one_touch_option", tags) {
    run_golden_test("Equity_OneTouch_Option.xml");
}

TEST_CASE("golden_roundtrip_equity_digital_option", tags) {
    run_golden_test("Equity_Digital_Option.xml");
}

// =============================================================================
// EquityAsianOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_asian_option", tags) {
    run_golden_test("Equity_Asian_Option.xml");
}

// =============================================================================
// EquityOutperformanceOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_equity_outperformance_option", tags) {
    run_golden_test("Equity_OutperformanceOption.xml");
}
