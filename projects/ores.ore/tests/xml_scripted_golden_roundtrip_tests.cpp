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
 * @file xml_scripted_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for Scripted
 * instruments: AsianBasketOption, AverageStrikeBasketOption,
 * LookbackCallBasketOption, FlooredAverageCPIZCIIS, IrregularYYIIS, and
 * MovingMaxYYIIS.
 */

namespace {

const std::string_view test_suite("ores.ore.scripted.golden.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][golden][scripted]");

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
// AsianBasketOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_scripted_basket_option", tags) {
    run_golden_test("Scripted_BasketOption.xml");
}

// =============================================================================
// AverageStrikeBasketOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_scripted_basket_option2", tags) {
    run_golden_test("Scripted_BasketOption2.xml");
}

// =============================================================================
// LookbackCallBasketOption tests
// =============================================================================

TEST_CASE("golden_roundtrip_scripted_basket_option3", tags) {
    run_golden_test("Scripted_BasketOption3.xml");
}

// =============================================================================
// FlooredAverageCPIZCIIS tests
// =============================================================================

TEST_CASE("golden_roundtrip_scripted_floored_average_cpi_zc_yyiis", tags) {
    run_golden_test("Scripted_FlooredAverageCPIZCYYIIS.xml");
}

// =============================================================================
// IrregularYYIIS tests
// =============================================================================

TEST_CASE("golden_roundtrip_scripted_irregular_yyiis", tags) {
    run_golden_test("Scripted_IrregularYYIIS.xml");
}

// =============================================================================
// MovingMaxYYIIS tests
// =============================================================================

TEST_CASE("golden_roundtrip_scripted_moving_maximum_yyiis", tags) {
    run_golden_test("Scripted_MovingMaximumYYIIS.xml");
}
