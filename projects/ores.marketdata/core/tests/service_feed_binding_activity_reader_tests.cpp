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
#include "ores.marketdata.core/service/feed_binding_activity_reader.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

/**
 * @file service_feed_binding_activity_reader_tests.cpp
 * @brief What one binding's activity says, from the row recorded for it.
 *
 * The read over the bindings and the rows needs a database. These tests cover
 * what it builds from each answer: only the caller's party's bindings are kept, a source that has
 * written names its series and the time of its newest observation, and a source that has written
 * nothing names neither.
 */

namespace {

const std::string tags("[marketdata][service][feed_binding_activity_reader]");

using namespace ores::marketdata;
using ores::marketdata::service::feed_binding_activity_reader;

repository::feed_binding_activity_record recorded(const std::string& series_id,
                                                  std::chrono::system_clock::time_point at) {
    return {boost::uuids::string_generator{}(series_id), at};
}


domain::feed_binding binding(const std::string& party_id, const std::string& source) {
    domain::feed_binding b;
    b.party_id = boost::uuids::string_generator{}(party_id);
    b.source_name = source;
    return b;
}

}

TEST_CASE("only the bindings of the caller's party are read from a tenant's bindings", tags) {
    const std::string mine = "11111111-1111-4111-8111-111111111111";
    const std::string other = "22222222-2222-4222-8222-222222222222";
    const auto kept = feed_binding_activity_reader::bindings_of_party(
        {binding(mine, "synthetic.bloomberg.audusd"),
         binding(other, "synthetic.bloomberg.eurusd"),
         binding(mine, "synthetic.bloomberg.nzdusd")},
        boost::uuids::string_generator{}(mine));

    REQUIRE(kept.size() == 2);
    CHECK(kept[0].source_name == "synthetic.bloomberg.audusd");
    CHECK(kept[1].source_name == "synthetic.bloomberg.nzdusd");
}

TEST_CASE("a party with no bindings of its own reads none", tags) {
    const auto kept = feed_binding_activity_reader::bindings_of_party(
        {binding("22222222-2222-4222-8222-222222222222", "synthetic.bloomberg.eurusd")},
        boost::uuids::string_generator{}("11111111-1111-4111-8111-111111111111"));

    CHECK(kept.empty());
}

TEST_CASE("a source that has written names its series and its last tick", tags) {
    using namespace std::chrono;
    const auto at = sys_days{year{2026} / 10 / 12} + hours{23} + minutes{50} + seconds{26};
    const auto activity = feed_binding_activity_reader::activity_of(
        "synthetic.bloomberg.audusd", recorded("5d0f4a1e-9c2b-4a6e-8f31-0a7b1c2d3e4f", at));

    CHECK(activity.source_name == "synthetic.bloomberg.audusd");
    REQUIRE(activity.series_id.has_value());
    CHECK(boost::uuids::to_string(*activity.series_id) == "5d0f4a1e-9c2b-4a6e-8f31-0a7b1c2d3e4f");
    REQUIRE(activity.last_tick_at.has_value());
    CHECK(*activity.last_tick_at == at);
}

TEST_CASE("a source that has written nothing names neither a series nor a tick", tags) {
    const auto activity =
        feed_binding_activity_reader::activity_of("synthetic.bloomberg.nzdusd", std::nullopt);

    CHECK(activity.source_name == "synthetic.bloomberg.nzdusd");
    CHECK_FALSE(activity.series_id.has_value());
    CHECK_FALSE(activity.last_tick_at.has_value());
}
