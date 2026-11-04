/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.
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
#include "ores.telemetry.service/app/nats_monitoring_replies.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <cstdint>
#include <limits>
#include <string>

using namespace ores::telemetry::service::app;
using ores::utility::codec::object::read_json;

TEST_CASE("a /varz reply reads the counters among the fields it does not name",
          "[nats_monitoring]") {
    const std::string body = R"({
  "server_id": "NDXQ",
  "version": "2.14.0",
  "now": "2026-11-01T19:00:00.000Z",
  "cpu": 0.5,
  "http_req_stats": {"/varz": 3},
  "in_msgs": 18446744073709551615,
  "out_msgs": 42,
  "in_bytes": 9223372036854775808,
  "out_bytes": 7,
  "connections": 12,
  "mem": 33554432,
  "slow_consumers": 1,
  "tags": ["a", "b"]
})";

    const auto r = read_json<varz_response>(body);

    CHECK(r.in_msgs == std::numeric_limits<std::uint64_t>::max());
    CHECK(r.out_msgs == 42);
    CHECK(r.in_bytes == 9223372036854775808ULL);
    CHECK(r.out_bytes == 7);
    CHECK(r.connections == 12);
    CHECK(r.mem == 33554432);
    CHECK(r.slow_consumers == 1);
}

TEST_CASE("a /jsz reply without account details reads as none", "[nats_monitoring]") {
    const auto r = read_json<jsz_response>(R"({"server_id": "NDXQ", "streams": 0})");

    CHECK_FALSE(r.account_details.has_value());
}

TEST_CASE("a /jsz reply reads each account's streams", "[nats_monitoring]") {
    const std::string body = R"({
  "streams": 2,
  "account_details": [
    {"name": "$G", "id": "$G", "stream_detail": [
      {"name": "ORES_EVENTS", "created": "2026-11-01T00:00:00Z",
       "config": {"subjects": ["x.>"]},
       "state": {"messages": 5, "bytes": 18446744073709551615,
                 "first_seq": 1, "consumer_count": 2}},
      {"name": "ORES_LOGS", "state": {"messages": 0, "bytes": 0, "consumer_count": 0}}
    ]}
  ]
})";

    const auto r = read_json<jsz_response>(body);

    REQUIRE(r.account_details.has_value());
    REQUIRE(r.account_details->size() == 1);
    const auto& streams = r.account_details->front().stream_detail;
    REQUIRE(streams.size() == 2);
    CHECK(streams[0].name == "ORES_EVENTS");
    CHECK(streams[0].state.messages == 5);
    CHECK(streams[0].state.bytes == std::numeric_limits<std::uint64_t>::max());
    CHECK(streams[0].state.consumer_count == 2);
    CHECK(streams[1].name == "ORES_LOGS");
    CHECK(streams[1].state.messages == 0);
}

TEST_CASE("a reply that is not JSON is refused with a reason", "[nats_monitoring]") {
    CHECK_THROWS_WITH(read_json<varz_response>("<html>502 Bad Gateway</html>"),
                      Catch::Matchers::ContainsSubstring("Not JSON"));
}
