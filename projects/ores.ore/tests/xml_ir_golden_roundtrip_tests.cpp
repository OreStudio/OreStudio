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
 * @file xml_ir_golden_roundtrip_tests.cpp
 * @brief Thing 1: ores.ore XSD serialization fidelity tests.
 *
 * For each IR/CCS/FRA/CapFloor example file from
 * external/ore/examples/Products/Example_Trades/:
 *   1. Parse the ORE XML into ores.ore domain types.
 *   2. Serialize back to canonical XML.
 *   3. Compare against the committed golden file in
 *      assets/test_data/golden_dataset/Products/Example_Trades/.
 *      If no golden file exists, write it (bootstrap mode) and pass.
 *
 * This test detects regressions in the ores.ore XSD serialization layer.
 * It does NOT involve ORES domain types or mappers.
 */

namespace {

const std::string_view test_suite("ores.ore.ir.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][ir]");

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

/**
 * @brief Run a golden-file round-trip test for a single portfolio XML file.
 *
 * If the golden file does not exist, it is created from the first serialization
 * (bootstrap). On subsequent runs the serialized output must match exactly.
 */
void test_golden_roundtrip(const std::string& filename) {
    auto lg(make_logger(test_suite));
    const auto src = example_path(filename);
    const auto golden = golden_path(filename);

    BOOST_LOG_SEV(lg, debug) << "Testing golden roundtrip for: " << filename;

    using ores::platform::filesystem::file;
    const std::string content = file::read_content(src);

    portfolio p;
    ores::ore::domain::load_data(content, p);
    const std::string serialized = ores::ore::domain::save_data(p);

    if (!std::filesystem::exists(golden)) {
        BOOST_LOG_SEV(lg, info) << "No golden file found, writing: " << golden;
        std::filesystem::create_directories(golden.parent_path());
        std::ofstream out(golden);
        out << serialized;
        BOOST_LOG_SEV(lg, info) << "Golden file written: " << golden;
        // First run: bootstrap. Test passes.
        return;
    }

    const std::string expected = file::read_content(golden);
    if (serialized != expected) {
        BOOST_LOG_SEV(lg, error) << "Golden diff for: " << filename;
        BOOST_LOG_SEV(lg, error) << "Expected length: " << expected.size()
                                 << "  Got: " << serialized.size();
    }
    CHECK(serialized == expected);
    BOOST_LOG_SEV(lg, info) << "Golden roundtrip passed: " << filename;
}

} // namespace

// =============================================================================
// Swap tests
// =============================================================================

TEST_CASE("golden_roundtrip_ir_swap_vanilla", tags) {
    test_golden_roundtrip("IR_Swap_Vanilla.xml");
}

TEST_CASE("golden_roundtrip_ir_ois_swap", tags) {
    test_golden_roundtrip("IR_OIS_Swap.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_amortising", tags) {
    test_golden_roundtrip("IR_Swap_Amortising.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_amortising_notionals", tags) {
    test_golden_roundtrip("IR_Swap_Amortising_Notionals.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_amortising_amortizations", tags) {
    test_golden_roundtrip("IR_Swap_Amortising_Amortizations.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_amortising_amortizations_accretion", tags) {
    test_golden_roundtrip("IR_Swap_Amortising_Amortizations_Accretion.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_cms", tags) {
    test_golden_roundtrip("IR_Swap_CMS.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_bma", tags) {
    test_golden_roundtrip("IR_Swap_BMA.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_cny_repofix", tags) {
    test_golden_roundtrip("IR_Swap_CNY-REPOFIX.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_custom_fixings", tags) {
    test_golden_roundtrip("IR_Swap_Custom_Fixings.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_zero_coupon_fixed", tags) {
    test_golden_roundtrip("IR_Swap_zero_coupon_fixed_leg.xml");
}

TEST_CASE("golden_roundtrip_ir_swap_zero_coupon_floating", tags) {
    test_golden_roundtrip("IR_Swap_zero_coupon_floating_leg.xml");
}

TEST_CASE("golden_roundtrip_ir_basis_swap", tags) {
    test_golden_roundtrip("IR_Basis_Swap_Single_Currency.xml");
}

// =============================================================================
// Cross-currency swap tests
// =============================================================================

TEST_CASE("golden_roundtrip_irfx_ccs_rebalancing", tags) {
    test_golden_roundtrip("IRFX_Cross_Currency_Swap_rebalancing.xml");
}

TEST_CASE("golden_roundtrip_irfx_ccs_non_rebalancing", tags) {
    test_golden_roundtrip("IRFX_Cross_Currency_Swap_non_rebalancing.xml");
}

TEST_CASE("golden_roundtrip_irfx_ccs_ndirs", tags) {
    test_golden_roundtrip("IRFX_Cross_Currency_Swap_NDIRS.xml");
}

// =============================================================================
// FRA tests
// =============================================================================

TEST_CASE("golden_roundtrip_ir_fra", tags) {
    test_golden_roundtrip("IR_FRA.xml");
}

// =============================================================================
// CapFloor tests
// =============================================================================

TEST_CASE("golden_roundtrip_ir_cap_ibor", tags) {
    test_golden_roundtrip("IR_Cap_on_IBOR.xml");
}

TEST_CASE("golden_roundtrip_ir_cap_ibor_amortising", tags) {
    test_golden_roundtrip("IR_Cap_on_IBOR_amortising.xml");
}

TEST_CASE("golden_roundtrip_ir_collar_ibor", tags) {
    test_golden_roundtrip("IR_Collar_on_IBOR.xml");
}

TEST_CASE("golden_roundtrip_ir_cap_ibor_mxntiie", tags) {
    test_golden_roundtrip("IR_Cap_on_IBOR_MXNTIIE.xml");
}

TEST_CASE("golden_roundtrip_ir_capfloor_fedfunds", tags) {
    test_golden_roundtrip("IR_CapFloor_on_FedFunds_amortising.xml");
}

TEST_CASE("golden_roundtrip_ir_cap_cms", tags) {
    test_golden_roundtrip("IR_Cap_on_CMS.xml");
}

TEST_CASE("golden_roundtrip_ir_ois_capped_floored", tags) {
    test_golden_roundtrip("IR_OIS_capped_floored.xml");
}
