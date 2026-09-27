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
#include <catch2/catch_test_macros.hpp>
#include <string>

// The keys are literals a reader has to agree with: the ore service downloads
// what the reporting service uploaded, and the bucket name is a contract with
// the storage server, which answers 404 for one it does not know.

using namespace ores::reporting::service;

namespace {
const std::string tags("[service][storage]");
}

TEST_CASE("the report bucket is the one the storage server knows", tags) {
    CHECK(report_data_bucket == "report-data");
}

TEST_CASE("each gathered set lands under the instance that gathered it", tags) {
    const std::string instance("11111111-2222-3333-4444-555555555555");
    CHECK(trades_storage_key(instance) == "11111111-2222-3333-4444-555555555555/trades.msgpack");
    CHECK(market_data_storage_key(instance) ==
          "11111111-2222-3333-4444-555555555555/market_data.msgpack");
}

TEST_CASE("two executions never share a key", tags) {
    CHECK(trades_storage_key("a") != trades_storage_key("b"));
}
