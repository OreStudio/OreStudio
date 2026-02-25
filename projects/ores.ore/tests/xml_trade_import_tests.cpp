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
#include "ores.ore/xml/importer.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][trade_import]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

}

using ores::ore::xml::importer;
using ores::trading::domain::trade;
using namespace ores::logging;

// =============================================================================
// validate_trade tests
// =============================================================================

TEST_CASE("validate_trade_with_all_required_fields", tags) {
    auto lg(make_logger(test_suite));

    trade t;
    t.external_id = "Swap_1";
    t.trade_type = "Swap";

    const auto errors = importer::validate_trade(t);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(errors.empty());
}

TEST_CASE("validate_trade_with_missing_external_id", tags) {
    auto lg(make_logger(test_suite));

    trade t;
    t.trade_type = "Swap";

    const auto errors = importer::validate_trade(t);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("External ID is required"));
}

TEST_CASE("validate_trade_with_missing_trade_type", tags) {
    auto lg(make_logger(test_suite));

    trade t;
    t.external_id = "Swap_1";

    const auto errors = importer::validate_trade(t);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("Trade type is required"));
}

TEST_CASE("validate_trade_with_multiple_errors", tags) {
    auto lg(make_logger(test_suite));

    trade t{};
    const auto errors = importer::validate_trade(t);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("External ID is required"));
    CHECK(errors.contains("Trade type is required"));
}

// =============================================================================
// import_portfolio tests
// =============================================================================

TEST_CASE("import_portfolio_from_minimal_swap", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(
        "examples/MinimalSetup/Input/portfolio_swap.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto trades = importer::import_portfolio(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << trades.size() << " trades";

    REQUIRE(trades.size() == 1);

    const auto& t = trades.front();
    CHECK(t.external_id == "Swap_20y");
    CHECK(t.trade_type == "Swap");
    CHECK(t.netting_set_id == "CPTY_A");
    CHECK(t.lifecycle_event == "New");
    CHECK(t.modified_by == "ores");
    CHECK(t.change_reason_code == "system.external_data_import");
}

TEST_CASE("import_portfolio_from_example_1", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/portfolio.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto trades = importer::import_portfolio(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << trades.size() << " trades";

    REQUIRE(trades.size() == 12);

    // First trade should be a Swap.
    const auto& first = trades.front();
    CHECK(first.external_id == "Swap_20");
    CHECK(first.trade_type == "Swap");
    CHECK(first.netting_set_id == "CPTY_A");

    // Verify all trades have the same counterparty netting set.
    for (const auto& t : trades) {
        CHECK(t.netting_set_id == "CPTY_A");
    }
}

TEST_CASE("import_portfolio_from_minimal_swaptions", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(
        "examples/MinimalSetup/Input/portfolio_swaptions.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto trades = importer::import_portfolio(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << trades.size() << " trades";

    REQUIRE(!trades.empty());

    // All trades should be Swaptions.
    for (const auto& t : trades) {
        CHECK(t.trade_type == "Swaption");
    }
}

TEST_CASE("import_portfolio_all_trades_pass_validation", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/portfolio.xml");
    const auto trades = importer::import_portfolio(f);
    REQUIRE(!trades.empty());

    for (const auto& t : trades) {
        const auto errors = importer::validate_trade(t);
        INFO("Trade " << t.external_id << " failed validation: " << errors);
        CHECK(errors.empty());
    }

    BOOST_LOG_SEV(lg, debug) << "All " << trades.size()
                             << " imported trades pass validation";
}
