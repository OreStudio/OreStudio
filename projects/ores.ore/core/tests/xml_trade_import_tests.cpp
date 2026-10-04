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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <fstream>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][trade_import]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

}

using ores::ore::xml::importer;
using ores::ore::xml::trade_import_item;
using ores::trading::domain::swap_instrument_data;
using ores::trading::domain::fx_instrument_variant;
using ores::trading::domain::fx_forward_instrument;
using ores::trading::domain::bond_instrument_data;
using namespace ores::logging;

// =============================================================================
// import_portfolio tests
// =============================================================================

TEST_CASE("import_portfolio_from_minimal_swap", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/MinimalSetup/Input/portfolio_swap.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto items = importer::import_portfolio_with_context(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << items.size() << " trades";

    REQUIRE(items.size() == 1);

    const auto& item = items.front();
    CHECK(item.ore_id == "Swap_20y");
    CHECK(item.anchor.trade_type == "Swap");
    REQUIRE(item.envelope.has_value());
    CHECK(item.envelope->netting_set_id == "CPTY_A");
    CHECK(item.activity_type_code == "new_booking");
}

TEST_CASE("import_portfolio_leaves_the_ids_to_the_planner", tags) {
    auto lg(make_logger(test_suite));

    const auto items = importer::import_portfolio_with_context(
        ore_path("examples/MinimalSetup/Input/portfolio_swap.xml"));
    REQUIRE(items.size() == 1);

    const auto nil = boost::uuids::nil_uuid();
    const auto& item = items.front();
    CHECK(item.anchor.id == nil);
    CHECK(item.anchor.party_id == nil);
    CHECK_FALSE(item.anchor.counterparty_id.has_value());
    CHECK(item.booking.trade_id == nil);
    CHECK(item.booking.book_id == nil);
}

TEST_CASE("import_portfolio_from_example_1", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/portfolio.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto items = importer::import_portfolio_with_context(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << items.size() << " trades";

    REQUIRE(items.size() == 12);

    // First trade should be a Swap.
    const auto& first = items.front();
    CHECK(first.ore_id == "Swap_20");
    CHECK(first.anchor.trade_type == "Swap");

    // Verify all trades have the same counterparty netting set.
    for (const auto& item : items) {
        REQUIRE(item.envelope.has_value());
        CHECK(item.envelope->netting_set_id == "CPTY_A");
    }
}

TEST_CASE("import_portfolio_from_minimal_swaptions", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/MinimalSetup/Input/portfolio_swaptions.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto items = importer::import_portfolio_with_context(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << items.size() << " trades";

    REQUIRE(!items.empty());

    // All trades should be Swaptions.
    for (const auto& item : items) {
        CHECK(item.anchor.trade_type == "Swaption");
    }
}

TEST_CASE("import_portfolio_gives_every_trade_an_ore_id_and_type", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/portfolio.xml");
    const auto items = importer::import_portfolio_with_context(f);
    REQUIRE(!items.empty());

    for (const auto& item : items) {
        INFO("Trade " << item.ore_id);
        CHECK_FALSE(item.ore_id.empty());
        CHECK_FALSE(item.anchor.trade_type.empty());
    }

    BOOST_LOG_SEV(lg, debug) << "All " << items.size() << " imported trades pass validation";
}

TEST_CASE("import_portfolio_all_ore_example_files_can_be_parsed", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ores::testing::project_root::resolve("external/ore/examples");

    // Collect all XML files that contain a <Portfolio> element, sorted for
    // reproducible ordering.
    std::vector<std::filesystem::path> portfolio_files;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file())
            continue;
        if (entry.path().extension() != ".xml")
            continue;

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
            FAIL_CHECK("Exception importing " << file.filename() << ": " << e.what());
            continue;
        }

        const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::steady_clock::now() - t0)
                            .count();

        trade_count += static_cast<int>(items.size());
        BOOST_LOG_SEV(lg, info) << file.filename() << " -> " << items.size() << " trades in " << ms
                                << "ms";

        for (const auto& item : items) {
            INFO("File: " << file.filename() << "  Trade: " << item.ore_id);
            CHECK_FALSE(item.ore_id.empty());
            CHECK_FALSE(item.anchor.trade_type.empty());
        }
    }

    BOOST_LOG_SEV(lg, info) << "Total trades imported across all " << portfolio_files.size()
                            << " files: " << trade_count;
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
    INFO("Trade type: " << item.anchor.trade_type);
    REQUIRE(std::holds_alternative<swap_instrument_data>(item.instrument));

    // Mint the id as the planner would; the trade id is the instrument's key,
    // and the test verifies the wiring.
    boost::uuids::random_generator gen;
    item.anchor.id = gen();
    ores::trading::domain::stamp_ids(item.instrument, item.anchor.id);

    const auto& r = std::get<swap_instrument_data>(item.instrument);
    const auto instr_id =
        std::visit([](const auto& instr) { return instr.identity.trade_id; }, r.instrument);
    CHECK(instr_id == item.anchor.id);
    CHECK(!r.legs.empty());
    for (const auto& leg : r.legs)
        CHECK(leg.identity.trade_id == instr_id);

    BOOST_LOG_SEV(lg, info) << "Swap instrument mapped. Legs: " << r.legs.size();
}

TEST_CASE("import_portfolio_with_context_fx_forward_has_instrument", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("FX_Forward.xml");
    auto items = importer::import_portfolio_with_context(f);
    REQUIRE(items.size() == 1);

    auto& item = items.front();
    INFO("Trade type: " << item.anchor.trade_type);
    REQUIRE(std::holds_alternative<fx_instrument_variant>(item.instrument));

    // Mint the id as the planner would; the trade id is the instrument's key,
    // and the test verifies the wiring.
    boost::uuids::random_generator gen;
    item.anchor.id = gen();
    ores::trading::domain::stamp_ids(item.instrument, item.anchor.id);

    const auto& r = std::get<fx_instrument_variant>(item.instrument);
    const auto& instr = std::get<fx_forward_instrument>(r);
    CHECK(instr.identity.trade_id == item.anchor.id);
    CHECK(!instr.bought_currency.empty());
    CHECK(!instr.sold_currency.empty());

    BOOST_LOG_SEV(lg, info) << "FX instrument mapped: " << instr.bought_currency << "/"
                            << instr.sold_currency;
}

TEST_CASE("import_portfolio_with_context_bond_has_instrument", tags) {
    auto lg(make_logger(test_suite));

    const auto f = example_path("Cash_Bonds.xml");
    auto items = importer::import_portfolio_with_context(f);
    REQUIRE(!items.empty());

    // First trade in the portfolio must be a bond.
    auto& item = items.front();
    INFO("Trade type: " << item.anchor.trade_type);
    REQUIRE(std::holds_alternative<bond_instrument_data>(item.instrument));

    // Mint the id as the planner would; the trade id is the instrument's key,
    // and the test verifies the wiring.
    boost::uuids::random_generator gen;
    item.anchor.id = gen();
    ores::trading::domain::stamp_ids(item.instrument, item.anchor.id);

    const auto& r = std::get<bond_instrument_data>(item.instrument);
    CHECK(r.instrument.identity.trade_id == item.anchor.id);
    CHECK(r.instrument.issue_id == r.issue.issue_id);
    CHECK(!r.issue.issuer.empty());

    BOOST_LOG_SEV(lg, info) << "Bond instrument mapped. Issuer: " << r.issue.issuer;
}

TEST_CASE("unmapped_trade_type_is_monostate", tags) {
    auto lg(make_logger(test_suite));

    // Every document in the examples tree maps now that Ascot has a
    // mapper, so the guard builds the trade the tree cannot supply. A
    // BondPosition is a sub-trade type that no document states on its
    // own and no dispatcher covers.
    ores::ore::domain::trade t;
    t.TradeType = ores::ore::domain::oreTradeType::BondPosition;
    CHECK(
        std::holds_alternative<std::monostate>(ores::ore::domain::trade_mapper::map_instrument(t)));

    BOOST_LOG_SEV(lg, info) << "Unmapped trade type correctly yields monostate";
}
