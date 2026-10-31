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
#include "ores.marketdata.core/repository/market_state_payload_repository.hpp"
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string payload_tags("[repository][market_state_payload]");

using ores::marketdata::repository::market_state_payload_repository;
using ores::marketdata::service::market_state_file;
using ores::marketdata::service::market_state_run;
using ores::marketdata::service::read_market_state;
using ores::testing::database_helper;

// A cube narrow enough to read, wide enough that the stored text is not a word.
std::string cube_file() {
    std::string csv = "#Date,Scenario,Numeraire";
    for (int i = 0; i < 40; ++i)
        csv += ",DiscountCurve/USD/" + std::to_string(i);
    csv += "\n";
    for (const auto* scenario : {"BASE", "UP", "DOWN"}) {
        csv += "2026-10-31," + std::string(scenario) + ",1.0";
        for (int i = 0; i < 40; ++i)
            csv += ",0.98" + std::to_string(i);
        csv += "\n";
    }
    return csv;
}

}

TEST_CASE("a stored market state reads back with its index and its payload", payload_tags) {
    database_helper h;
    const auto body = cube_file();
    const auto run = market_state_run{.party_id = boost::uuids::random_generator()(),
                                      .report_instance_id = boost::uuids::random_generator()(),
                                      .report_definition_id = boost::uuids::random_generator()(),
                                      .as_of = std::chrono::sys_days{
                                          std::chrono::year{2026} / 10 / 31}};

    const auto outcome =
        read_market_state({market_state_file{.name = "stress_scenarios.csv", .body = body}}, run);
    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.payloads.size() == 1);

    market_state_payload_repository repo;
    repo.insert(h.context(), outcome.payloads);

    const auto read = repo.read_latest(h.context());
    const auto stored = std::ranges::find_if(read, [&](const auto& one) {
        return one.report_instance_id == run.report_instance_id;
    });
    REQUIRE(stored != read.end());

    // The index survives the round trip, and so does the payload it describes.
    CHECK(stored->file_name == "stress_scenarios.csv");
    CHECK(stored->family == "stress_scenarios");
    CHECK(stored->row_count == 3);
    CHECK(stored->column_count == 43);
    CHECK_FALSE(stored->keys_truncated);
    CHECK(stored->column_names.find("DiscountCurve/USD/39") != std::string::npos);
    CHECK(stored->key_values.find("BASE") != std::string::npos);
    CHECK(stored->payload == outcome.payloads.front().payload);
    CHECK(stored->content_sha256 == outcome.payloads.front().content_sha256);
}
