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

/**
 * @file service_feed_binding_activity_reader_tests.cpp
 * @brief What one binding's activity says, from the newest observation of its source.
 *
 * The read over the bindings and the observations needs a database. These tests cover
 * what it builds from each answer: a source that has written names its series and the
 * time of its newest observation, and a source that has written nothing names neither.
 */

namespace {

const std::string tags("[marketdata][service][feed_binding_activity_reader]");

using namespace ores::marketdata;
using ores::marketdata::service::feed_binding_activity_reader;

domain::market_observation observation(const std::string& series_id,
                                       std::chrono::system_clock::time_point at) {
    domain::market_observation obs;
    obs.series_id = boost::uuids::string_generator{}(series_id);
    obs.observation_datetime = at;
    return obs;
}

}

TEST_CASE("a source that has written names its series and its last tick", tags) {
    using namespace std::chrono;
    const auto at = sys_days{year{2026} / 10 / 12} + hours{23} + minutes{50} + seconds{26};
    const auto activity = feed_binding_activity_reader::activity_of(
        "synthetic.bloomberg.audusd", observation("5d0f4a1e-9c2b-4a6e-8f31-0a7b1c2d3e4f", at));

    CHECK(activity.source_name == "synthetic.bloomberg.audusd");
    CHECK(activity.series_id == "5d0f4a1e-9c2b-4a6e-8f31-0a7b1c2d3e4f");
    CHECK(activity.last_tick_at == "2026-10-12 23:50:26Z");
}

TEST_CASE("a source that has written nothing names neither a series nor a tick", tags) {
    const auto activity =
        feed_binding_activity_reader::activity_of("synthetic.bloomberg.nzdusd", std::nullopt);

    CHECK(activity.source_name == "synthetic.bloomberg.nzdusd");
    CHECK(activity.series_id.empty());
    CHECK(activity.last_tick_at.empty());
}
