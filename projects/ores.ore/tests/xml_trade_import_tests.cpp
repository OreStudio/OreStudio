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

#include <fstream>
#include <chrono>
#include <algorithm>
#include <filesystem>
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][trade_import]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

}

using ores::ore::xml::importer;
using ores::ore::xml::trade_import_item;
using ores::trading::domain::swap_instrument_data;
using ores::trading::domain::fx_instrument_variant;
using ores::trading::domain::fx_forward_instrument;
using ores::trading::domain::bond_instrument;
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

    const auto items = importer::import_portfolio_with_context(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << items.size() << " trades";

    REQUIRE(items.size() == 1);

    const auto& t = items.front().trade;
    CHECK(t.external_id == "Swap_20y");
    CHECK(t.trade_type == "Swap");
    CHECK(t.netting_set_id == "CPTY_A");
    CHECK(t.activity_type_code == "new_booking");
    CHECK(t.modified_by == "ores");
    CHECK(t.change_reason_code == "system.external_data_import");
}

TEST_CASE("import_portfolio_from_example_1", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/portfolio.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto items = importer::import_portfolio_with_context(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << items.size() << " trades";

    REQUIRE(items.size() == 12);

    // First trade should be a Swap.
    const auto& first = items.front().trade;
    CHECK(first.external_id == "Swap_20");
    CHECK(first.trade_type == "Swap");
    CHECK(first.netting_set_id == "CPTY_A");

    // Verify all trades have the same counterparty netting set.
    for (const auto& item : items) {
        CHECK(item.trade.netting_set_id == "CPTY_A");
    }
}

TEST_CASE("import_portfolio_from_minimal_swaptions", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(
        "examples/MinimalSetup/Input/portfolio_swaptions.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto items = importer::import_portfolio_with_context(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << items.size() << " trades";

    REQUIRE(!items.empty());

    // All trades should be Swaptions.
    for (const auto& item : items) {
        CHECK(item.trade.trade_type == "Swaption");
    }
}

TEST_CASE("import_portfolio_all_trades_pass_validation", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/portfolio.xml");
    const auto items = importer::import_portfolio_with_context(f);
    REQUIRE(!items.empty());

    for (const auto& item : items) {
        const auto errors = importer::validate_trade(item.trade);
        INFO("Trade " << item.trade.external_id << " failed validation: " << errors);
        CHECK(errors.empty());
    }

    BOOST_LOG_SEV(lg, debug) << "All " << items.size()
                             << " imported trades pass validation";
}

TEST_CASE("import_portfolio_all_ore_example_files_can_be_parsed", tags) {
    auto lg(make_logger(test_suite));

    const auto root =
        ores::testing::project_root::resolve("external/ore/examples");

    // Collect all XML files that contain a <Portfolio> element, sorted for
    // reproducible ordering.
    std::vector<std::filesystem::path> portfolio_files;
    for (const auto& entry :
             std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file()) continue;
        if (entry.path().extension() != ".xml") continue;

        std::ifstream ifs(entry.path(), std::ios::binary);
        std::string buf(4096, '\0');
        ifs.read(buf.data(), static_cast<std::streamsize>(buf.size()));
        buf.resize(static_cast<std::size_t>(ifs.gcount()));
        if (buf.find("<Portfolio>") != std::string::npos)
            portfolio_files.push_back(entry.path());
    }
    std::sort(portfolio_files.begin(), portfolio_files.end());

    BOOST_LOG_SEV(lg, info) << "Found " << portfolio_files.size()
                            << " portfolio files in ORE examples";
    REQUIRE(portfolio_files.size() > 300);

    int trade_count = 0;
    for (const auto& file : portfolio_files) {
        BOOST_LOG_SEV(lg, info) << "Importing: " << file;
        const auto t0 = std::chrono::steady_clock::now();

        std::vector<trade_import_item> items;
        try {
            items = importer::import_portfolio_with_context(file);
        } catch (const std::exception& e) {
            FAIL_CHECK("Exception importing " << file.filename()
                       << ": " << e.what());
            continue;
        }

        const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - t0).count();

        trade_count += static_cast<int>(items.size());
        BOOST_LOG_SEV(lg, info) << file.filename() << " -> "
                                << items.size() << " trades in " << ms << "ms";

        for (const auto& item : items) {
            const auto errors = importer::validate_trade(item.trade);
            INFO("File: " << file.filename()
                 << "  Trade: " << item.trade.external_id);
            CHECK(errors.empty());
        }
    }

    BOOST_LOG_SEV(lg, info) << "Total trades imported across all "
                            << portfolio_files.size() << " files: "
                            << trade_count;
}

// =============================================================================
// import_portfolio_with_context instrument mapping tests
// =============================================================================

TEST_CASE("import_portfolio_with_context_swap_has_instrument", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("IR_Swap_Vanilla.xml");
    auto items = importer::import_portfolio_with_context(f);
    REQUIRE(items.size() == 1);

    auto& item = items.front();
    INFO("Trade type: " << item.trade.trade_type);
    REQUIRE(std::holds_alternative<swap_instrument_data>(item.instrument));

    // Mint UUIDs as the planner would; tests verify the wiring is correct.
    boost::uuids::random_generator gen;
    item.trade.id = gen();
    const auto instr_uuid = gen();
    ores::trading::domain::stamp_ids(item.instrument, instr_uuid, item.trade.id);
    item.trade.instrument_id = instr_uuid;

    const auto& r = std::get<swap_instrument_data>(item.instrument);
    const auto instr_id = std::visit(
        [](const auto& instr) { return instr.instrument_id; }, r.instrument);
    const auto trade_id_opt = std::visit(
        [](const auto& instr) { return instr.trade_id; }, r.instrument);
    CHECK(instr_id != item.trade.id);
    REQUIRE(trade_id_opt.has_value());
    CHECK(*trade_id_opt == item.trade.id);
    CHECK(item.trade.instrument_id == instr_id);
    CHECK(item.trade.product_type == ores::trading::domain::product_type::swap);
    CHECK(!r.legs.empty());
    for (const auto& leg : r.legs)
        CHECK(leg.instrument_id == instr_id);

    BOOST_LOG_SEV(lg, info) << "Swap instrument mapped. Legs: " << r.legs.size();
}

TEST_CASE("import_portfolio_with_context_fx_forward_has_instrument", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("FX_Forward.xml");
    auto items = importer::import_portfolio_with_context(f);
    REQUIRE(items.size() == 1);

    auto& item = items.front();
    INFO("Trade type: " << item.trade.trade_type);
    REQUIRE(std::holds_alternative<fx_instrument_variant>(item.instrument));

    // Mint UUIDs as the planner would; tests verify the wiring is correct.
    boost::uuids::random_generator gen;
    item.trade.id = gen();
    const auto instr_uuid = gen();
    ores::trading::domain::stamp_ids(item.instrument, instr_uuid, item.trade.id);
    item.trade.instrument_id = instr_uuid;

    const auto& r = std::get<fx_instrument_variant>(item.instrument);
    const auto& instr = std::get<fx_forward_instrument>(r);
    CHECK(instr.instrument_id != item.trade.id);
    REQUIRE(instr.trade_id.has_value());
    CHECK(*instr.trade_id == item.trade.id);
    CHECK(item.trade.instrument_id == instr.instrument_id);
    CHECK(item.trade.product_type == ores::trading::domain::product_type::fx);
    CHECK(!instr.bought_currency.empty());
    CHECK(!instr.sold_currency.empty());

    BOOST_LOG_SEV(lg, info) << "FX instrument mapped: "
                            << instr.bought_currency << "/"
                            << instr.sold_currency;
}

TEST_CASE("import_portfolio_with_context_bond_has_instrument", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("Cash_Bonds.xml");
    auto items = importer::import_portfolio_with_context(f);
    REQUIRE(!items.empty());

    // First trade in the portfolio must be a bond.
    auto& item = items.front();
    INFO("Trade type: " << item.trade.trade_type);
    REQUIRE(std::holds_alternative<bond_instrument>(item.instrument));

    // Mint UUIDs as the planner would; tests verify the wiring is correct.
    boost::uuids::random_generator gen;
    item.trade.id = gen();
    const auto instr_uuid = gen();
    ores::trading::domain::stamp_ids(item.instrument, instr_uuid, item.trade.id);
    item.trade.instrument_id = instr_uuid;

    const auto& r = std::get<bond_instrument>(item.instrument);
    CHECK(r.instrument_id != item.trade.id);
    CHECK(r.trade_id == item.trade.id);
    CHECK(item.trade.instrument_id == r.instrument_id);
    CHECK(item.trade.product_type == ores::trading::domain::product_type::bond);
    CHECK(!r.issuer.empty());

    BOOST_LOG_SEV(lg, info) << "Bond instrument mapped. Issuer: " << r.issuer;
}

TEST_CASE("import_portfolio_with_context_unmapped_type_is_monostate", tags) {
    auto lg(make_logger(test_suite));

    // Ascot is not yet mapped, so it should produce monostate.
    const auto f = example_path("Cash_Ascot.xml");
    const auto items = importer::import_portfolio_with_context(f);
    REQUIRE(!items.empty());

    // The first trade in Cash_Ascot.xml is an Ascot — not yet mapped.
    const auto& item = items.front();
    INFO("Trade type: " << item.trade.trade_type);
    CHECK(std::holds_alternative<std::monostate>(item.instrument));

    BOOST_LOG_SEV(lg, info) << "Unmapped trade type '" << item.trade.trade_type
                            << "' correctly yields monostate";
}
