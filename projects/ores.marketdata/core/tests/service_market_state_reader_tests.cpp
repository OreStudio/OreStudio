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
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include "ores.ore.core/results/payload_index.hpp"
#include "ores.utility/compression/gzip.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <boost/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <string>
#include <vector>

namespace {

const std::string market_state_tags("[marketdata][market_state_reader]");

using ores::marketdata::service::market_state_file;
using ores::marketdata::service::market_state_run;
using ores::marketdata::service::read_market_state;

const auto priced_at = std::chrono::sys_days{std::chrono::year{2026} / 10 / 31};

market_state_run the_run() {
    return market_state_run{.party_id = boost::uuids::random_generator()(),
                            .report_instance_id = boost::uuids::random_generator()(),
                            .report_definition_id = boost::uuids::random_generator()(),
                            .as_of = priced_at};
}

std::string cube_file() {
    return "#Date,Scenario,Numeraire,DiscountCurve/USD/0,DiscountCurve/USD/1\n"
           "2026-10-31,BASE,1.0,0.99,0.98\n"
           "2026-10-31,UP,1.0,1.01,1.00\n"
           "2026-11-30,BASE,1.0,0.98,0.97\n";
}

}

TEST_CASE("a cube is stored as one compressed payload with an index of its keys",
          market_state_tags) {
    const auto run = the_run();
    const auto result = boost::uuids::random_generator()();
    const auto body = cube_file();
    const auto outcome = read_market_state(
        {market_state_file{.result_id = result, .name = "stress_scenarios.csv", .body = body}},
        run);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.payloads.size() == 1);
    CHECK(outcome.left_alone.empty());

    const auto& payload = outcome.payloads.front();
    CHECK(payload.family == "stress_scenarios");
    CHECK(payload.file_name == "stress_scenarios.csv");
    CHECK(payload.party_id == run.party_id);
    CHECK(payload.report_instance_id == run.report_instance_id);
    CHECK(payload.report_definition_id == run.report_definition_id);
    CHECK(payload.as_of == priced_at);
    CHECK(payload.row_count == 3);
    CHECK(payload.column_count == 5);
    CHECK_FALSE(payload.keys_truncated);
    CHECK(payload.result_id == result);

    // The headings and the keys are read out of the index the reader wrote, so a
    // screen can find a slice without unpacking the file.
    const auto columns = boost::json::parse(payload.column_names).as_array();
    REQUIRE(columns.size() == 5);
    CHECK(columns[0].as_string() == "Date");

    const auto keys = boost::json::parse(payload.key_values).as_object();
    const auto& dates = keys.at("Date").as_array();
    CHECK(dates.size() == 2);
    CHECK(dates[0].as_string() == "2026-10-31");
    CHECK(dates[1].as_string() == "2026-11-30");
    CHECK(keys.at("Scenario").as_array().size() == 2);

    // The payload is the file itself: compressed, then held as text.
    const auto compressed_bytes =
        ores::utility::convert::base64_converter::convert(payload.payload);
    const auto unpacked = ores::utility::compression::gzip_decompress(std::span<const char>(
        reinterpret_cast<const char*>(compressed_bytes.data()), compressed_bytes.size()));
    CHECK(std::string(unpacked.begin(), unpacked.end()) == body);
    CHECK(payload.payload_bytes ==
          static_cast<int>(ores::utility::compression::gzip_compress(
                               std::span<const char>(body.data(), body.size()))
                               .size()));
    CHECK(payload.content_sha256 == ores::utility::crypto::sha256::hex_digest(body));
}

TEST_CASE("a run's results are left alone rather than stored as market state", market_state_tags) {
    const auto outcome = read_market_state(
        {market_state_file{.name = "npv.csv", .body = "#TradeId,NPV\nT1,1.0\n"},
         market_state_file{.name = "something-else.txt", .body = "not a table\n"}},
        the_run());

    CHECK(outcome.refusals.empty());
    CHECK(outcome.payloads.empty());
    REQUIRE(outcome.left_alone.size() == 2);
    CHECK(outcome.left_alone[0] == "npv.csv");
    CHECK(outcome.left_alone[1] == "something-else.txt");
}

TEST_CASE("a market-state file with no heading stops the run's state", market_state_tags) {
    const auto outcome =
        read_market_state({market_state_file{.name = "rawcube.csv", .body = ""}}, the_run());

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("rawcube") != std::string::npos);
    CHECK(outcome.payloads.empty());
}

TEST_CASE("each result of a run keeps its own file of the same name", market_state_tags) {
    const auto first = boost::uuids::random_generator()();
    const auto second = boost::uuids::random_generator()();
    const auto outcome = read_market_state(
        {market_state_file{.result_id = first, .name = "netcube.csv", .body = cube_file()},
         market_state_file{.result_id = second, .name = "netcube.csv", .body = cube_file()}},
        the_run());

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.payloads.size() == 2);
    CHECK(outcome.payloads[0].result_id == first);
    CHECK(outcome.payloads[1].result_id == second);
}

TEST_CASE("a result that states the same file twice is refused", market_state_tags) {
    const auto result = boost::uuids::random_generator()();
    const auto outcome = read_market_state(
        {market_state_file{.result_id = result, .name = "netcube.csv", .body = cube_file()},
         market_state_file{.result_id = result, .name = "netcube.csv", .body = cube_file()}},
        the_run());

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("twice") != std::string::npos);
}
