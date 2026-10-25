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
#include "ores.marketdata.api/datum/series_stream.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[datum][series_stream]");

const std::string zero_series =
    "oresmd://ir/EUR?type=series&instrument=zero&quote=rate&curve_id=abc&day_counter=A365";

}

TEST_CASE("a_stream_id_is_the_same_every_time_it_is_computed", tags) {
    const auto once = ores::marketdata::datum::stream_id("ore", zero_series);
    const auto again = ores::marketdata::datum::stream_id("ore", zero_series);
    CHECK(once == again);
    // The name is pinned, because the ingest and the read must build the same
    // one, and a change to the namespace or to the spelling strands every
    // stored tick under an id nothing derives again.
    CHECK(boost::uuids::to_string(once) == "c14edc6c-6aae-502b-90ee-995e524f35da");
}

TEST_CASE("a_stream_id_differs_for_each_provider", tags) {
    CHECK(ores::marketdata::datum::stream_id("ore", zero_series) !=
          ores::marketdata::datum::stream_id("bloomberg", zero_series));
}

TEST_CASE("a_stream_id_differs_for_each_series_uri", tags) {
    const auto other =
        "oresmd://ir/EUR?type=series&instrument=zero&quote=rate&curve_id=xyz&day_counter=A365";
    CHECK(ores::marketdata::datum::stream_id("ore", zero_series) !=
          ores::marketdata::datum::stream_id("ore", other));
}

TEST_CASE("a_provider_code_and_a_series_uri_never_name_two_streams", tags) {
    // The provider code's length is part of the name, so moving characters
    // across the boundary names a different stream.
    CHECK(ores::marketdata::datum::stream_id("ab", "c") !=
          ores::marketdata::datum::stream_id("a", "bc"));
}
