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
 * @file xml_fx_golden_roundtrip_tests.cpp
 * @brief Thing 1: XSD serialization fidelity golden file tests for FX trades.
 *
 * For each example:
 *   1. Parse ORE XML using ores.ore XSD types.
 *   2. Serialize back to XML (canonical form).
 *   3. On first run: write canonical XML as the golden file (bootstrap).
 *   4. On subsequent runs: compare against the golden file.
 *
 * A git diff on a golden file is an explicit sign-off that the serialization
 * change is intentional.
 */

namespace {

const std::string_view test_suite("ores.ore.fx.golden.roundtrip.tests");
const std::string tags("[ore][xml][golden][roundtrip][fx]");

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

TEST_CASE("golden_roundtrip_fx_forward", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Forward.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Forward golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_swap", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Swap.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Swap golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_option_european", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Option_European.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Option_European golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_option_american", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Option_American.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Option_American golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_digital_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Digital_Option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Digital_Option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Barrier_Option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Barrier_Option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_digital_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Digital_Barrier_Option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Digital_Barrier_Option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_double_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_DoubleBarrierOption.xml");
    BOOST_LOG_SEV(lg, info) << "FX_DoubleBarrierOption golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_double_touch_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_DoubleTouch_Option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_DoubleTouch_Option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_european_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_FxEuropeanBarrierOption.xml");
    BOOST_LOG_SEV(lg, info) << "FX_FxEuropeanBarrierOption golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_kiko_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_KIKO_Barrier_Option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_KIKO_Barrier_Option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_no_touch_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_NoTouch_option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_NoTouch_option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_one_touch_option", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_OneTouch_option.xml");
    BOOST_LOG_SEV(lg, info) << "FX_OneTouch_option golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_average_forward", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Average_Forward.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Average_Forward golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_Variance_Swap.xml");
    BOOST_LOG_SEV(lg, info) << "FX_Variance_Swap golden roundtrip passed";
}

TEST_CASE("golden_roundtrip_fx_worst_of_basket_swap", tags) {
    auto lg(make_logger(test_suite));
    run_golden_test("FX_WorstOfBasketSwap.xml");
    BOOST_LOG_SEV(lg, info) << "FX_WorstOfBasketSwap golden roundtrip passed";
}
