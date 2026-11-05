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
#include "ores.ore.core/log/engine_log.hpp"
#include "ores.reporting.core/results/canonical_log_reader.hpp"
#include "ores.reporting.core/service/engine_log_row.hpp"
#include "ores.testing/test_files.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// The node projects the engine's log onto the canonical rows and the service
// reads them back. The two halves live in different components, so the pair is
// checked here on the log of the demo's failed Headline Position run: that log
// is the one that recorded a wall of text instead of the trade failures.

namespace {

using ores::ore::log::canonical_log_csv;
using ores::ore::log::parse_engine_log_json;
using ores::ore::log::parse_engine_log_text;
using ores::reporting::results::read_canonical_log_csv;
using ores::reporting::service::engine_log_row;
using ores::testing::read_file;

/** The structured log the node uploaded for the demo's failed run, verbatim. */
const std::string demo_log_path =
    "projects/ores.reporting/core/tests/data/demo_failed_run_log_structured.json";

/**
 * @brief The rows a node writes for one log, read back as the service reads them.
 *
 * This is the node's own pair of calls and the service's own call around them,
 * in the order the objects travel: the node marshals the rows it projected and
 * the service loads what the object states.
 */
std::vector<engine_log_row> rows_of(const std::string& log, const std::string& source) {
    const auto events = source == "log.txt" ? parse_engine_log_text(log, source) :
                                              parse_engine_log_json(log, source);
    const auto read = read_canonical_log_csv(canonical_log_csv(events));
    REQUIRE(read.refusals.empty());
    return read.rows;
}

const engine_log_row* row_of_trade(const std::vector<engine_log_row>& rows,
                                   const std::string& trade_id) {
    const auto found = std::ranges::find(rows, trade_id, &engine_log_row::subject_id);
    return found == rows.end() ? nullptr : &*found;
}

}

TEST_CASE("the demo's failed run arrives as one row for each trade the engine could not build",
          "[reporting][log][demo]") {
    const auto rows = rows_of(read_file(ores::testing::project_root::resolve(demo_log_path)),
                              "log_structured.json");

    // Every row the engine wrote in that run is a trade it failed to build.
    REQUIRE(rows.size() == 24);
    CHECK(std::ranges::all_of(rows, [](const auto& row) {
        return row.severity == "error" && row.scope == "Trade" && row.subject == "trade";
    }));
    CHECK(std::ranges::none_of(rows, [](const auto& row) { return row.subject_id.empty(); }));

    // One row, named: the trade the run failed, the engine's reason for it, and
    // the exception the engine raised while reading that trade.
    const auto* trade = row_of_trade(rows, "00cf3e0f-29e1-53f9-9e85-eac72dcff6ca");
    REQUIRE(trade != nullptr);
    CHECK(trade->message == "XML Node is NULL (expected OptionData)");
    CHECK(trade->detail == R"({"exceptionType":"Error parsing Trade XML"})");
    CHECK(trade->source == "log_structured.json");
}

TEST_CASE("the failures survive the canonical rows the node writes, quote and comma intact",
          "[reporting][log][demo]") {
    const auto rows = rows_of(read_file(ores::testing::project_root::resolve(demo_log_path)),
                              "log_structured.json");

    // A message the engine quotes: an unquoted row would split on its comma, and
    // a field a reader could not place would be refused whole.
    const auto quoted = std::ranges::find(
        rows, std::string("Barrier type \"KnockOut\" not recognized"), &engine_log_row::message);
    CHECK(quoted != rows.end());
}

TEST_CASE("an engine that writes only a text log still states a row for each failed trade",
          "[reporting][log][demo]") {
    // The fallback the node reads when the engine writes no structured log: the
    // same messages, embedded in the level-timed lines of its text log.
    const std::string text_log =
        "ALERT    [2026-Nov-03 22:40:07.022192]          (OREData/ored/utilities/log.cpp:214) : "
        "StructuredMessage { \"category\": \"Error\", \"group\": \"Trade\", \"message\": \"XML "
        "Node "
        "is NULL (expected OptionData)\", \"sub_fields\": [ { \"name\": \"exceptionType\", "
        "\"value\": \"Error parsing Trade XML\" }, { \"name\": \"tradeId\", \"value\": "
        "\"00cf3e0f-29e1-53f9-9e85-eac72dcff6ca\" }, { \"name\": \"tradeType\", \"value\": "
        "\"CallableSwap\" } ] }\n"
        "WARNING  [2026-Nov-03 22:40:07.022279]    (OREData/ored/portfolio/portfolio.cpp:103) : "
        "Added trade id 00cf3e0f-29e1-53f9-9e85-eac72dcff6ca type Failed for original trade type "
        "CallableSwap\n";

    const auto rows = rows_of(text_log, "log.txt");

    REQUIRE(rows.size() == 2);
    CHECK(rows[0].severity == "error");
    CHECK(rows[0].scope == "Trade");
    CHECK(rows[0].subject == "trade");
    CHECK(rows[0].subject_id == "00cf3e0f-29e1-53f9-9e85-eac72dcff6ca");
    CHECK(rows[0].message == "XML Node is NULL (expected OptionData)");
    // A line the engine wrote without a group is read as it says, and the
    // recorder is what decides a row that names no scope is not a failure of a
    // part of the run.
    CHECK(rows[1].severity == "warning");
    CHECK(rows[1].scope.empty());
}
