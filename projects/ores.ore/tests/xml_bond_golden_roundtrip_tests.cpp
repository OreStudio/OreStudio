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

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_bond_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for bond trades.
 */

namespace {

const std::string_view test_suite("ores.ore.bond.golden.roundtrip.tests");
const std::string tags("[ore][xml][golden][roundtrip][bond]");

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
}

} // namespace

TEST_CASE("golden_roundtrip_bond_cash_bonds", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Cash_Bonds.xml");
    BOOST_LOG_SEV(lg, info) << "Cash_Bonds golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_cash_bond_repo", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Cash_BondRepo_and_Bond.xml");
    BOOST_LOG_SEV(lg, info) << "Cash_BondRepo_and_Bond golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_convertible", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Cash_ConvertibleBond.xml");
    BOOST_LOG_SEV(lg, info) << "Cash_ConvertibleBond golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_ascot", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Cash_Ascot.xml");
    BOOST_LOG_SEV(lg, info) << "Cash_Ascot golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_credit_bond_forward", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Credit_Bond_Forward.xml");
    BOOST_LOG_SEV(lg, info) << "Credit_Bond_Forward golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_credit_bond_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Credit_BondOption.xml");
    BOOST_LOG_SEV(lg, info) << "Credit_BondOption golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_credit_bond_trs", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Credit_Bond_TRS.xml");
    BOOST_LOG_SEV(lg, info) << "Credit_Bond_TRS golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_bond_credit_bond_trs_indexings", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("Credit_Bond_TRS_with_Indexings.xml");
    BOOST_LOG_SEV(lg, info) << "Credit_Bond_TRS_with_Indexings golden roundtrip passed";
}
