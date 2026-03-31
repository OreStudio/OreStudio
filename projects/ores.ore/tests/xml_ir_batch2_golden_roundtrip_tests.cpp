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
 * @file xml_ir_batch2_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for Batch 2
 * IR types: Swaption, CallableSwap, FlexiSwap, BalanceGuaranteedSwap, and
 * advanced Swap variants (CMS Spread, NDIRS, etc.).
 */

namespace {

const std::string_view test_suite("ores.ore.ir.batch2.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][ir][batch2]");

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
// Swaption tests
// =============================================================================

TEST_CASE("golden_roundtrip_ir_swaption_european", tags) {
    run_golden_test("IR_Swaption_European.xml");
}

TEST_CASE("golden_roundtrip_ir_swaption_bermudan", tags) {
    run_golden_test("IR_Swaption_Bermudan.xml");
}

// =============================================================================
// Callable / exotic swap tests
// =============================================================================

TEST_CASE("golden_roundtrip_ir_callable_swap_bermudan", tags) {
    run_golden_test("IR_Callable_Swap_Bermudan.xml");
}

TEST_CASE("golden_roundtrip_ir_flexi_swap", tags) {
    run_golden_test("IR_Flexi_Swap.xml");
}

TEST_CASE("golden_roundtrip_ir_balance_guaranteed_swap", tags) {
    run_golden_test("IR_BalanceGuaranteedSwap.xml");
}

// =============================================================================
// Advanced Swap variants (all use SwapData — existing forward_swap handles them)
// =============================================================================

TEST_CASE("golden_roundtrip_ir_dur_adjusted_cms", tags) {
    run_golden_test("IR_DurAdjusted_CMS.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_cms_spread_option", tags) {
    run_golden_test("IR_Swap_CMS_Spread_Option.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_digital_cms_spread_option", tags) {
    run_golden_test("IR_Swap_Digital_CMS_Spread_Option.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_capped_amortising", tags) {
    run_golden_test("IR_Swap_Capped_Amortising_.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_ndirs_non_xccy", tags) {
    run_golden_test("IR_Swap_NDIRS_nonXCCY.xml");
}
