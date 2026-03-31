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
 * @file xml_credit_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for Credit
 * instruments: CreditDefaultSwap, IndexCreditDefaultSwap,
 * IndexCreditDefaultSwapOption, CreditLinkedSwap,
 * RiskParticipationAgreement, and CdoData.
 */

namespace {

const std::string_view test_suite("ores.ore.credit.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][credit]");

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
// CreditDefaultSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_credit_default_swap", tags) {
    run_golden_test("Credit_Default_Swap.xml");
}

// =============================================================================
// IndexCreditDefaultSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_credit_index_cds", tags) {
    run_golden_test("Credit_Index_Credit_Default_Swap.xml");
}

TEST_CASE("golden_roundtrip_credit_index_cds_bespoke_basket", tags) {
    run_golden_test("Credit_Index_Credit_Default_Swap_Bespoke_Basket.xml");
}

// =============================================================================
// IndexCreditDefaultSwapOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_credit_index_cds_option", tags) {
    run_golden_test("Credit_Index_CDS_Option.xml");
}

// =============================================================================
// CreditLinkedSwap tests
// =============================================================================

TEST_CASE("golden_roundtrip_credit_linked_swap", tags) {
    run_golden_test("Credit_CreditLinkedSwap.xml");
}

// =============================================================================
// RiskParticipationAgreement tests
// =============================================================================

TEST_CASE("golden_roundtrip_credit_rpa_vanilla_swap", tags) {
    run_golden_test("Credit_RiskParticipationAgreement_on_Vanilla_Swap.xml");
}

TEST_CASE("golden_roundtrip_credit_rpa_callable_swap", tags) {
    run_golden_test("Credit_RiskParticipationAgreement_on_CallableSwap.xml");
}

// =============================================================================
// CdoData tests
// =============================================================================

TEST_CASE("golden_roundtrip_credit_synthetic_cdo", tags) {
    run_golden_test("Credit_Synthetic_CDO_refdata.xml");
}
