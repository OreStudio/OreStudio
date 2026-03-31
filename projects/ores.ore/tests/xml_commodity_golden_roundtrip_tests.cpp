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
 * @file xml_commodity_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for Commodity
 * instruments: Swap, Forward, Option, AveragePriceOption, OptionStrip,
 * Swaption, and VarianceSwap.
 *
 * CommoditySwap files use SwapData (already dispatched by the swap mapper);
 * the remaining types have their own CommodityXxxData elements.
 */

namespace {

const std::string_view test_suite("ores.ore.commodity.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][commodity]");

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
// CommoditySwap tests (SwapData with commodity legs)
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_basis_swap_nymex_cl", tags) {
    run_golden_test("Commodity_Basis_Swap_NYMEX_CL.xml");
}

TEST_CASE("golden_roundtrip_commodity_swap_lme_al_avg_alt", tags) {
    run_golden_test("Commodity_Swap_LME_AL_AVG_ALT.xml");
}

TEST_CASE("golden_roundtrip_commodity_swap_nymex_a7q", tags) {
    run_golden_test("Commodity_Swap_NYMEX_A7Q.xml");
}

// =============================================================================
// CommodityForward tests
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_forward", tags) {
    run_golden_test("Commodity_Forward.xml");
}

// =============================================================================
// CommodityOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_option", tags) {
    run_golden_test("Commodity_Option.xml");
}

// =============================================================================
// CommodityAveragePriceOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_apo_nymex_cl", tags) {
    run_golden_test("Commodity_APO_NYMEX_CL.xml");
}

// =============================================================================
// CommodityOptionStrip tests
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_apo_strip_nymex_csx", tags) {
    run_golden_test("Commodity_APO_Strip_NYMEX_CSX.xml");
}

TEST_CASE("golden_roundtrip_commodity_option_strip_nymex_ng", tags) {
    run_golden_test("Commodity_Option_Strip_NYMEX_NG.xml");
}

// =============================================================================
// CommoditySwaption tests
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_swaption_nymex_ng", tags) {
    run_golden_test("Commodity_Swaption_NYMEX_NG.xml");
}

// =============================================================================
// CommodityVarianceSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_commodity_variance_swap", tags) {
    run_golden_test("Commodity_Variance_Swap.xml");
}
