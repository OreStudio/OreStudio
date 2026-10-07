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
#include "ores.reporting.core/service/execution_storage_plan.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The ore service downloads what the reporting service uploaded, so both sides
// have to agree on the bucket and on the shape of the key. The protocol is
// where that agreement is written down.

using namespace ores::reporting::service;

namespace {
const std::string tags("[service][storage]");
}

TEST_CASE("the plan builds against the protocol's one bucket", tags) {
    CHECK(ores::storage::api::object_keys::ores_bucket == "ores");
}

TEST_CASE("each gathered set lands under the instance that gathered it", tags) {
    const std::string instance("11111111-2222-3333-4444-555555555555");
    CHECK(trades_storage_key(instance) ==
          "reporting/runs/11111111-2222-3333-4444-555555555555/trades.msgpack");
    CHECK(market_data_storage_key(instance) ==
          "reporting/runs/11111111-2222-3333-4444-555555555555/market_data.txt");
    CHECK(fixings_storage_key(instance) ==
          "reporting/runs/11111111-2222-3333-4444-555555555555/fixings.txt");
}

TEST_CASE("every key the plan builds is one the protocol parses", tags) {
    const std::string instance("11111111-2222-3333-4444-555555555555");

    const auto trades = ores::storage::api::object_keys::parse(trades_storage_key(instance));
    REQUIRE(trades.has_value());
    CHECK(trades->service == "reporting");
    CHECK(trades->purpose == "runs");
    CHECK(trades->id == instance);
    CHECK(trades->name == "trades.msgpack");

    const auto market_data =
        ores::storage::api::object_keys::parse(market_data_storage_key(instance));
    REQUIRE(market_data.has_value());
    CHECK(market_data->name == "market_data.txt");
}

TEST_CASE("the two sets one run gathers stay apart", tags) {
    const std::string instance("11111111-2222-3333-4444-555555555555");
    CHECK(trades_storage_key(instance) != market_data_storage_key(instance));
}

TEST_CASE("two executions never share a key", tags) {
    CHECK(trades_storage_key("a") != trades_storage_key("b"));
}
