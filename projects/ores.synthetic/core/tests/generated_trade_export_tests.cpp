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
#include "ore_example_conventions.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.core/xml/importer.hpp"
#include "ores.ore.core/xml/value_differ.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include "ores.trading.core/service/instrument_batch_writer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <format>
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
using namespace ores::synthetic::tests;
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

/// The classes a planner draws, as the strings the scope and the planners take.
const std::vector<std::string> asset_classes = [] {
    std::vector<std::string> codes;
    for (const auto code : planned_asset_classes())
        codes.emplace_back(code);
    return codes;
}();

/*
 * A scope of the class on one book. A planner draws its codes by the slot's
 * position, so a book of twice its code count states each code twice.
 */
domain::trade_generation_scope scope_of(const std::string& asset_class_code) {
    auto scope = trade_generation_scope_recipe();
    scope.config_id = export_config_id;
    scope.party_id = export_party_id;
    scope.asset_class_code = asset_class_code;
    scope.trades_per_book =
        trades_per_type * static_cast<std::uint32_t>(planner_for(asset_class_code).code_count());
    scope.book_ids = {book};
    scope.counterparty_ids = {export_bank, export_second_bank};
    scope.issuer_ids = {export_bank, export_second_bank};
    scope.traders = {domain::trade_population_trader{export_trader_account, "trader.one"}};
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
    ores::utility::codec::apply_fields(
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
    ores::utility::codec::apply_fields(
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
                 * A bond's security is written before its trade, and the export
                 * reader joins it and the trade's own rows back through
                 * assemble_bond. The slot carries the security beside the
                 * batch, and the batch holds only this trade's rows.
                 */
                for (auto& bond : request.instruments.bond_instruments) {
                    if (slot.bond_issue)
                        bond.issue = *slot.bond_issue;
                    bond.call_dates = slot.bond_issue_call_dates;
                    bond.conversion_targets = slot.bond_issue_conversion_targets;
                    const auto& batch = request.instruments;
                    const auto first = [](const auto& rows) {
                        using row = std::decay_t<decltype(rows.front())>;
                        return rows.empty() ? std::optional<row>{} :
                                              std::optional<row>{rows.front()};
                    };
                    ores::trading::service::bond_trade_rows trade;
                    trade.legs = batch.bond_legs;
                    trade.amounts = batch.bond_leg_amounts;
                    trade.rates = batch.bond_leg_rates;
                    trade.schedules = batch.instrument_schedules;
                    trade.schedule_dates = batch.instrument_schedule_dates;
                    trade.option = first(batch.instrument_options);
                    trade.strike = first(batch.instrument_strikes);
                    trade.forward = first(batch.bond_forwards);
                    trade.bond_option = first(batch.bond_options);
                    trade.trs = first(batch.bond_trs);
                    trade.repo = first(batch.bond_repos);
                    trade.future = first(batch.bond_futures);
                    trade.ascot = first(batch.ascots);
                    trade.underlyings = batch.instrument_underlyings;
                    ores::trading::service::assemble_bond(
                        bond,
                        trade,
                        {.legs = slot.bond_issue_legs,
                         .amounts = slot.bond_issue_leg_amounts,
                         .rates = slot.bond_issue_leg_rates,
                         .schedules = slot.bond_issue_leg_schedules});
                }
                state_keys(request.instruments, slot.trade_id, slot.trade_type_code);
                auto& trades = by_type[slot.trade_type_code];
                /*
                 * The booking service refuses a batch whose arrays the trade
                 * type does not route, or whose children have no header, so
                 * such a trade is never stored and never exported.
                 */
                auto refusals = ores::trading::service::misrouted_arrays(request.instruments,
                                                                         slot.trade_type_code);
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
    return ores::platform::environment::environment::get_value("ORES_UPDATE_GENERATED_TRADES") ==
           "1";
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
    /*
     * Empty: every generated type now states what the schema requires of it, so
     * the writer refuses none of them.
     */
    static const std::set<std::string> known;
    return known;
}

/// The generated trade types written with no product data. Asserted exactly.
const std::set<std::string>& known_passthroughs() {
    /*
     * Empty: every type the generator books now writes a product. A type ORE
     * cannot carry has left the generator, so nothing is booked as a name with
     * no terms behind it.
     */
    static const std::set<std::string> known;
    return known;
}

/*
 * A trade type and the first element of a path every ORE example of it states
 * that its recorded trades leave out, one a line. Asserted exactly; a fix
 * removes its line.
 */
/// The lines of a known list in the test data, less comments.
std::set<std::string> known_lines(const std::string& name) {
    const auto path =
        ores::testing::project_root::resolve("projects/ores.synthetic/core/tests/data/" + name);
    std::set<std::string> known;
    std::istringstream lines(file::read_content(path));
    for (std::string line; std::getline(lines, line);)
        if (!line.empty() && !line.starts_with("#"))
            known.insert(line);
    return known;
}

std::set<std::string> known_missing_paths() {
    return known_lines("generated_trades_known_missing_paths.txt");
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

/*
 * A generated trade holds oresmd addresses, and the projection writes each as
 * ORE names it. The importer reads those names back into addresses through the
 * same mappers, so a recorded trade read and written again states the same
 * values. A name one side spells by hand, outside the mappers, breaks this.
 *
 * The families not yet built through builders still differ, each in its own
 * projection; their differences are known by type and path, and a family's
 * builder task removes its lines.
 */
TEST_CASE("every recorded trade reads back and writes the same values",
          "[synthetic][ore][generated]") {
    namespace xml = ores::ore::xml;
    std::size_t trades = 0;
    std::set<std::string> differences;
    for (const auto& entry : fs::directory_iterator(recorded_directory())) {
        if (entry.path().extension() != ".xml")
            continue;
        const auto items = xml::importer::import_portfolio_with_context(entry.path());
        std::vector<ores::trading::messaging::trade_export_item> exports;
        ores::trading::domain::instrument_batch batch;
        for (const auto& item : items) {
            const auto id = boost::uuids::string_generator()(
                std::format("00000000-0000-0000-0000-{:012}", ++trades));
            auto instrument = item.instrument;
            ores::trading::domain::stamp_ids(instrument, id);
            ores::trading::messaging::trade_export_item exported;
            exported.anchor = item.anchor;
            exported.anchor.id = id;
            exported.ore_id = item.ore_id;
            exported.envelope = item.envelope;
            exports.push_back(std::move(exported));
            ores::trading::domain::append_instrument(batch, instrument);
        }
        const auto recorded = document::read(file::read_content(entry.path()));
        const auto written = document::read(xml::exporter::export_portfolio(exports, batch));
        REQUIRE(recorded.has_value());
        REQUIRE(written.has_value());
        const auto before = xml::value_fingerprints_of(*recorded, "Trade", "id");
        const auto after = xml::value_fingerprints_of(*written, "Trade", "id");
        REQUIRE(before.size() == after.size());
        for (std::size_t i = 0; i < before.size(); ++i)
            for (const auto& difference : xml::diff_values(before[i].values, after[i].values)) {
                // The type and the path, without the values or the positions.
                const auto described = xml::describe(difference);
                std::string shape;
                bool in_index = false;
                for (const char c : described.substr(0, described.find(": '"))) {
                    if (c == '[')
                        in_index = true;
                    else if (c == ']')
                        in_index = false;
                    else if (!in_index)
                        shape += c;
                }
                differences.insert(entry.path().stem().string() + " " + shape);
            }
    }
    CHECK(trades > 100);
    check_known(differences, known_lines("generated_trades_known_roundtrip_differences.txt"));
}
