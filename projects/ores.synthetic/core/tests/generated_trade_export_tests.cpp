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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/value_differ.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.core/service/instrument_batch_writer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

/*
 * The corpus roundtrip proves that ORE's own example trades survive our model.
 * A generated trade reaches ORE XML on another path: the planner states a slot,
 * the writer builds the booking request from it, and the exporter writes the
 * request's instruments. The trades that path writes are kept in the
 * repository, one portfolio per trade type, so that a change to what the
 * generator books is a change to a file a reviewer reads.
 *
 * Two kinds of case follow. The recorded cases hold the files to the
 * generator: a trade that moves fails until its file is written again, which
 * ORES_UPDATE_GENERATED_TRADES=1 does. The structure cases judge the files.
 */

namespace {

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::synthetic;
using namespace ores::synthetic::service;
using ores::ore::xml::document;
using ores::platform::filesystem::file;

const boost::uuids::uuid export_config_id =
    boost::uuids::string_generator()("0cd68b09-6ebc-4180-a298-0cc386590f31");
const boost::uuids::uuid export_party_id =
    boost::uuids::string_generator()("11111111-1111-1111-1111-111111111111");
const boost::uuids::uuid book =
    boost::uuids::string_generator()("22222222-2222-2222-2222-222222222222");
const boost::uuids::uuid export_bank =
    boost::uuids::string_generator()("44444444-4444-4444-4444-444444444444");
const boost::uuids::uuid export_second_bank =
    boost::uuids::string_generator()("55555555-5555-5555-5555-555555555555");
const boost::uuids::uuid export_trader_account =
    boost::uuids::string_generator()("66666666-6666-6666-6666-666666666666");

/// How many trades of each type a recorded portfolio holds.
constexpr std::uint32_t trades_per_type = 2;

const std::vector<std::string> asset_classes = {
    "bond", "commodity", "credit", "equity", "fx", "inflation", "interest_rates", "scripted"};

/*
 * A scope of the class on one book. A planner draws its codes by the slot's
 * position, so a book of twice its code count states each code twice.
 */
domain::trade_generation_scope scope_of(const std::string& asset_class_code) {
    domain::trade_generation_scope scope;
    scope.config_id = export_config_id;
    scope.party_id = export_party_id;
    scope.asset_class_code = asset_class_code;
    scope.min_tenor_code = "1Y";
    scope.max_tenor_code = "10Y";
    scope.min_notional = 100000.0;
    scope.max_notional = 10000000.0;
    scope.as_of_date = std::chrono::year{2026} / 1 / 2;
    scope.seed = 7;
    scope.trades_per_book = trades_per_type *
        static_cast<std::uint32_t>(planner_for(asset_class_code).code_count());
    scope.book_ids = {book};
    scope.counterparty_ids = {export_bank, export_second_bank};
    scope.issuer_ids = {export_bank, export_second_bank};
    scope.traders = {domain::trade_population_trader{export_trader_account, "trader.one"}};
    scope.currency_code = "EUR";
    scope.equity_underlyings = {
        domain::equity_underlying_reference{.code = ".SPX", .kind = "index", .currency = "USD"},
        domain::equity_underlying_reference{.code = "MSFT.O", .kind = "share", .currency = "USD"},
        domain::equity_underlying_reference{
            .code = ".STOXX50E", .kind = "index", .currency = "EUR"},
        domain::equity_underlying_reference{
            .code = "GB0005405286", .kind = "share", .currency = "GBP"}};
    scope.commodities = {
        domain::commodity_reference{.code = "NYMEX:CL", .unit = "bbl", .quotation_currency = "USD"},
        domain::commodity_reference{
            .code = "NYMEX:NG", .unit = "MMBtu", .quotation_currency = "USD"}};
    scope.floating_index_codes = {"EUR-EURIBOR-6M", "EUR-EONIA", "EUR-ESTR", "USD-SOFR"};
    scope.inflation_index_codes = {"UKRPIX", "USCPIA", "BECPIHL"};
    scope.currency_pairs = {{.base = "EUR", .quote = "USD"}, {.base = "GBP", .quote = "USD"}};
    scope.credit_reference_entities = {"529900T8BM49AURSDO55", "5493001KJTIIGC8Y1R12"};
    return scope;
}

/*
 * The booking service states the trade's keys on each row as it writes it, so
 * the rows of a booking request carry none. The test states them, so that one
 * batch can hold the rows of many trades.
 */
void state_keys(ores::trading::domain::instrument_batch& batch,
                const boost::uuids::uuid& trade_id,
                const std::string& trade_type) {
    const ores::trading::service::instrument_keys keys{
        trade_id, trade_type, boost::uuids::nil_uuid()};
    std::apply(
        [&](const auto&... fields) {
            (
                [&] {
                    for (auto& row : batch.*(fields.member))
                        ores::trading::service::state_keys(row, keys);
                }(),
                ...);
        },
        wire_fields(std::type_identity<ores::trading::domain::instrument_batch>{}));
}

void append(ores::trading::domain::instrument_batch& into,
            const ores::trading::domain::instrument_batch& from) {
    std::apply(
        [&](const auto&... fields) {
            ((into.*(fields.member))
                 .insert((into.*(fields.member)).end(),
                         (from.*(fields.member)).begin(),
                         (from.*(fields.member)).end()),
             ...);
        },
        wire_fields(std::type_identity<ores::trading::domain::instrument_batch>{}));
}

/// The trades of one type as the exporter writes them, or why it refuses them.
struct recorded_type {
    std::string xml;
    std::string refused;
};

/// Each generated trade type, written as a portfolio of its own.
const std::map<std::string, recorded_type>& generated() {
    static const std::map<std::string, recorded_type> result = [] {
        struct type_trades {
            std::vector<ores::trading::messaging::trade_export_item> items;
            ores::trading::domain::instrument_batch instruments;
            std::string booking_refused;
        };
        std::map<std::string, type_trades> by_type;
        for (const auto& asset_class : asset_classes) {
            const auto scope = scope_of(asset_class);
            for (const auto& slot : planner_for(asset_class).plan(scope).slots) {
                auto request = trading_trade_population_writer::book_request_for(slot, scope);
                /*
                 * A bond's security is written before its trade and joined back
                 * by the export reader, so the slot carries it beside the batch.
                 */
                for (auto& bond : request.instruments.bond_instruments) {
                    if (slot.bond_issue)
                        bond.issue = *slot.bond_issue;
                    bond.call_dates = slot.bond_issue_call_dates;
                    bond.conversion_targets = slot.bond_issue_conversion_targets;
                }
                state_keys(request.instruments, slot.trade_id, slot.trade_type_code);
                auto& trades = by_type[slot.trade_type_code];
                /*
                 * The booking service refuses a batch whose arrays the trade
                 * type does not route, or whose children have no header, so
                 * such a trade is never stored and never exported.
                 */
                auto refusals = ores::trading::service::misrouted_arrays(
                    request.instruments, slot.trade_type_code);
                for (auto& orphan : ores::trading::service::orphan_arrays(request.instruments))
                    refusals.push_back(std::move(orphan));
                if (!refusals.empty()) {
                    std::string names;
                    for (const auto& name : refusals)
                        names += (names.empty() ? "" : ", ") + name;
                    trades.booking_refused += boost::uuids::to_string(slot.trade_id) +
                                              ": booking refuses the arrays " + names + "\n";
                    continue;
                }
                ores::trading::messaging::trade_export_item item;
                item.anchor = request.anchor;
                item.ore_id = boost::uuids::to_string(slot.trade_id);
                trades.items.push_back(std::move(item));
                append(trades.instruments, request.instruments);
            }
        }

        std::map<std::string, recorded_type> out;
        for (const auto& [type, trades] : by_type) {
            recorded_type recorded;
            recorded.refused = trades.booking_refused;
            if (trades.items.empty()) {
                out.emplace(type, std::move(recorded));
                continue;
            }
            /*
             * A portfolio whose every trade is refused is an error rather than
             * an empty document; the refusal is then what the type records.
             */
            try {
                auto written = ores::ore::xml::exporter::export_portfolio_leaving_out(
                    trades.items, trades.instruments);
                recorded.xml = std::move(written.xml);
                for (const auto& refused : written.refused)
                    recorded.refused += refused.trade_id + ": " + refused.message + "\n";
            } catch (const std::exception& e) {
                recorded.refused += std::string(e.what()) + "\n";
            }
            out.emplace(type, std::move(recorded));
        }
        return out;
    }();
    return result;
}

fs::path recorded_directory() {
    return ores::testing::project_root::resolve(
        "projects/ores.synthetic/core/tests/data/generated_trades");
}

fs::path xml_file(const std::string& type) {
    return recorded_directory() / (type + ".xml");
}

fs::path refused_file(const std::string& type) {
    return recorded_directory() / (type + ".refused.txt");
}

bool updating() {
    const auto* flag = std::getenv("ORES_UPDATE_GENERATED_TRADES");
    return flag != nullptr && std::string_view(flag) == "1";
}

/// The file's content, or empty when there is no file.
std::string recorded_content(const fs::path& path) {
    return fs::exists(path) ? file::read_content(path) : std::string{};
}

/// Lists a set, one entry a line, for a failure message.
std::string listed(const std::set<std::string>& entries) {
    std::string out;
    for (const auto& entry : entries)
        out += "\n  " + entry;
    return out;
}

/// What changed between two portfolios, trade by trade, one line each.
std::string differences(const std::string& recorded, const std::string& generated) {
    const auto before = document::read(recorded);
    const auto after = document::read(generated);
    if (!before || !after)
        return "\n  the recorded or the generated portfolio does not read.";
    std::map<std::string, std::vector<ores::ore::xml::value_entry>> written;
    for (auto& trade : ores::ore::xml::value_fingerprints_of(*after, "Trade", "id"))
        written[trade.key] = std::move(trade.values);
    std::string out;
    for (const auto& trade : ores::ore::xml::value_fingerprints_of(*before, "Trade", "id")) {
        const auto found = written.find(trade.key);
        if (found == written.end()) {
            out += "\n  trade " + trade.key + " is no longer generated";
            continue;
        }
        for (const auto& difference : ores::ore::xml::diff_values(trade.values, found->second))
            out += "\n  trade " + trade.key + ": " + ores::ore::xml::describe(difference);
        written.erase(found);
    }
    for (const auto& [key, values] : written)
        out += "\n  trade " + key + " is new";
    if (out.empty())
        out = "\n  the text differs in layout alone";
    return out;
}

/// The trade types the recorded directory holds a file for.
std::set<std::string> recorded_types() {
    std::set<std::string> out;
    if (!fs::exists(recorded_directory()))
        return out;
    for (const auto& entry : fs::directory_iterator(recorded_directory())) {
        auto name = entry.path().filename().string();
        for (const std::string_view suffix : {".refused.txt", ".xml"})
            if (name.ends_with(suffix)) {
                out.insert(name.substr(0, name.size() - suffix.size()));
                break;
            }
    }
    return out;
}

void write_recorded() {
    fs::create_directories(recorded_directory());
    for (const auto& type : recorded_types()) {
        fs::remove(xml_file(type));
        fs::remove(refused_file(type));
    }
    for (const auto& [type, recorded] : generated()) {
        if (!recorded.xml.empty())
            file::write_content(xml_file(type), recorded.xml);
        if (!recorded.refused.empty())
            file::write_content(refused_file(type), recorded.refused);
    }
}

/// The paths a trade states a value at, outside its envelope, without positions.
std::set<std::string> product_paths(const std::vector<ores::ore::xml::value_entry>& values) {
    std::set<std::string> out;
    for (const auto& value : values) {
        if (value.value.empty())
            continue;
        std::string shape;
        bool in_index = false;
        for (const char c : value.path) {
            if (c == '[')
                in_index = true;
            else if (c == ']')
                in_index = false;
            else if (!in_index)
                shape += c;
        }
        if (shape.starts_with("Trade/Envelope") || shape == "Trade/@id" ||
            shape == "Trade/TradeType")
            continue;
        out.insert(shape);
    }
    return out;
}

/// The recorded trades of a type, each as the paths it states.
std::vector<std::set<std::string>> recorded_trades(const std::string& type) {
    std::vector<std::set<std::string>> out;
    const auto content = recorded_content(xml_file(type));
    if (content.empty())
        return out;
    const auto doc = document::read(content);
    if (!doc)
        return out;
    for (const auto& trade : ores::ore::xml::value_fingerprints_of(*doc, "Trade", "id"))
        out.push_back(product_paths(trade.values));
    return out;
}

/*
 * The paths every ORE example of a trade type states, by type. A path one
 * example leaves out is optional to ORE, so only the paths all of them state
 * are a requirement.
 */
std::map<std::string, std::set<std::string>> sample_requirements() {
    const auto corpus =
        ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades");
    std::map<std::string, std::set<std::string>> out;
    for (const auto& entry : fs::recursive_directory_iterator(corpus)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (ores::ore::xml::detect_document_kind(entry.path()) !=
            ores::ore::xml::document_kind::portfolio)
            continue;
        const auto doc = document::read(file::read_content(entry.path()));
        if (!doc)
            continue;
        for (const auto& trade : ores::ore::xml::value_fingerprints_of(*doc, "Trade", "id")) {
            std::string type;
            for (const auto& value : trade.values)
                if (value.path == "Trade/TradeType")
                    type = value.value;
            const auto paths = product_paths(trade.values);
            const auto [found, inserted] = out.try_emplace(type, paths);
            if (inserted)
                continue;
            std::set<std::string> kept;
            for (const auto& path : found->second)
                if (paths.contains(path))
                    kept.insert(path);
            found->second = std::move(kept);
        }
    }
    return out;
}

/// Fails naming what the known list lacks and what it names in vain.
void check_known(const std::set<std::string>& found, const std::set<std::string>& known) {
    std::set<std::string> fresh;
    std::set<std::string> gone;
    for (const auto& entry : found)
        if (!known.contains(entry))
            fresh.insert(entry);
    for (const auto& entry : known)
        if (!found.contains(entry))
            gone.insert(entry);
    INFO("New:" << listed(fresh));
    INFO("No longer happening, so remove from the known list:" << listed(gone));
    CHECK(fresh.empty());
    CHECK(gone.empty());
}

/// The generated trade types the writer refuses. Asserted exactly.
const std::set<std::string>& known_refusals() {
    static const std::set<std::string> known = {
        "BalanceGuaranteedSwap",
        "EquityOptionPosition",
        /*
         * The planner states these products' terms as rows of their own, which
         * the export reader joins to the bond from the database. The batch the
         * test exports from has no such join, so the three are refused here
         * whatever the reports do; the database leg of the story judges them.
         */
        "BondFuture",
        "BondTRS",
        "ForwardBond",
    };
    return known;
}

/// The generated trade types written with no product data. Asserted exactly.
const std::set<std::string>& known_passthroughs() {
    static const std::set<std::string> known = {
        "CommodityAccumulator",
        "CommodityAsianOption",
        "CommodityBasketVarianceSwap",
        "CommodityBestEntryOption",
        "CommodityDigitalAveragePriceOption",
        "CommodityDigitalOption",
        "CommodityForwardVolatilityAgreement",
        "CommodityGenericBarrierOption",
        "CommodityPairwiseVarianceSwap",
        "CommodityPosition",
        "CommodityRainbowOption",
        "CommoditySpreadOption",
        "CommodityStrikeResettableOption",
        "CommodityTaRF",
        "CommodityWindowBarrierOption",
        "CommodityWorstOfBasketSwap",
        "CreditDefaultSwap",
        "CreditLinkedSwap",
        "IndexCreditDefaultSwap",
        "IndexCreditDefaultSwapOption",
        "IntradayPowerForward",
        "RiskParticipationAgreement",
    };
    return known;
}

/*
 * A trade type and the first element of a path every ORE example of it states
 * that its recorded trades leave out, one a line. Asserted exactly; a fix
 * removes its line.
 */
std::set<std::string> known_missing_paths() {
    const auto path = ores::testing::project_root::resolve(
        "projects/ores.synthetic/core/tests/data/generated_trades_known_missing_paths.txt");
    std::set<std::string> known;
    std::istringstream lines(file::read_content(path));
    for (std::string line; std::getline(lines, line);)
        if (!line.empty())
            known.insert(line);
    return known;
}

}

TEST_CASE("the generated trades are the ones recorded", "[synthetic][ore][generated]") {
    auto lg(make_logger(test_suite));
    if (updating()) {
        write_recorded();
        WARN("Wrote the generated trades to " << recorded_directory().string());
    }

    std::set<std::string> generated_types;
    for (const auto& [type, recorded] : generated()) {
        generated_types.insert(type);
        INFO("Trade type " << type << "; to accept a change, run with "
                           << "ORES_UPDATE_GENERATED_TRADES=1 and review the diff.");
        const auto xml = recorded_content(xml_file(type));
        if (xml != recorded.xml) {
            INFO("The portfolio changed:" << differences(xml, recorded.xml));
            CHECK(xml == recorded.xml);
        }
        CHECK(recorded_content(refused_file(type)) == recorded.refused);
    }
    std::set<std::string> stale;
    for (const auto& type : recorded_types())
        if (!generated_types.contains(type))
            stale.insert(type);
    INFO("Recorded types no longer generated:" << listed(stale));
    CHECK(stale.empty());
}

TEST_CASE("the writer refuses no recorded trade type", "[synthetic][ore][generated]") {
    auto lg(make_logger(test_suite));
    std::set<std::string> found;
    for (const auto& type : recorded_types())
        if (fs::exists(refused_file(type)))
            found.insert(type);
    check_known(found, known_refusals());
}

TEST_CASE("every recorded trade states its product", "[synthetic][ore][generated]") {
    auto lg(make_logger(test_suite));
    std::set<std::string> found;
    for (const auto& type : recorded_types())
        for (const auto& paths : recorded_trades(type))
            if (paths.empty())
                found.insert(type);
    check_known(found, known_passthroughs());
}

TEST_CASE("a recorded trade states every path the ORE examples of its type state",
          "[synthetic][ore][generated][external]") {
    auto lg(make_logger(test_suite));
    const auto requirements = sample_requirements();
    std::set<std::string> found;
    std::set<std::string> no_example;
    for (const auto& type : recorded_types()) {
        const auto required = requirements.find(type);
        if (required == requirements.end()) {
            no_example.insert(type);
            continue;
        }
        for (const auto& stated : recorded_trades(type)) {
            if (stated.empty())
                continue;
            /*
             * A block the trade leaves out leaves out every path beneath it, so
             * the difference is named by the first element of the path the
             * trade does not state at all.
             */
            std::set<std::string> stated_elements;
            for (const auto& path : stated)
                for (auto slash = path.find('/'); slash != std::string::npos;
                     slash = path.find('/', slash + 1))
                    stated_elements.insert(path.substr(0, slash));
            for (const auto& path : required->second) {
                if (stated.contains(path))
                    continue;
                auto missing = path;
                for (auto slash = path.find('/'); slash != std::string::npos;
                     slash = path.find('/', slash + 1)) {
                    if (!stated_elements.contains(path.substr(0, slash))) {
                        missing = path.substr(0, slash);
                        break;
                    }
                }
                found.insert(type + " " + missing);
            }
        }
    }
    INFO("Recorded types ORE has no example of:" << listed(no_example));
    check_known(found, known_missing_paths());
}
