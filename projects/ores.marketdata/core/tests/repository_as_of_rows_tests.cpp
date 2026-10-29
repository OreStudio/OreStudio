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
#include "ores.database/pq/text.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.marketdata.core/repository/as_of_rows.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <stdexcept>
#include <string>

namespace {

using ores::marketdata::repository::as_of_bucket_ordinal;
using ores::marketdata::repository::as_of_observation;
using ores::marketdata::repository::as_of_row;
using Catch::Matchers::ContainsSubstring;
using ores::database::repository::db_timestamp;

as_of_row observation_columns() {
    return {std::string("0b64b4b0-1c4b-4a0e-9a2f-3d5e6f7a8b9c"),
            std::string("ffffffff-ffff-ffff-ffff-ffffffffffff"),
            std::string("1b64b4b0-1c4b-4a0e-9a2f-3d5e6f7a8b9c"),
            std::string("2b64b4b0-1c4b-4a0e-9a2f-3d5e6f7a8b9c"),
            std::string("2026-10-05 09:00:00+00"),
            std::string("oresmd://fx/EUR/USD?type=quote&instrument=fx_spot&quote=rate"),
            std::string("1.0845"),
            std::nullopt,
            std::string("26772cc0-57d6-4fac-be36-2ae75eb56b6b"),
            std::string("2026-10-05 09:00:00+00"),
            std::string("9999-12-31 23:59:59+00")};
}

as_of_row bucket_row(std::optional<std::string> ordinal) {
    as_of_row row{std::move(ordinal)};
    const auto rest = observation_columns();
    row.insert(row.end(), rest.begin(), rest.end());
    return row;
}

}

TEST_CASE("an_as_of_row_maps_every_observation_column", "[repository][as_of]") {
    const auto e = as_of_observation(bucket_row("0"), 1, "read_as_of_buckets");

    CHECK(e.id == boost::lexical_cast<boost::uuids::uuid>(
                      std::string("0b64b4b0-1c4b-4a0e-9a2f-3d5e6f7a8b9c")));
    CHECK(e.series_id == boost::lexical_cast<boost::uuids::uuid>(
                             std::string("2b64b4b0-1c4b-4a0e-9a2f-3d5e6f7a8b9c")));
    CHECK(e.value == "1.0845");
    CHECK_FALSE(e.source.has_value());
    CHECK(e.source_id == boost::lexical_cast<boost::uuids::uuid>(
                             std::string("26772cc0-57d6-4fac-be36-2ae75eb56b6b")));
    CHECK(e.valid_from == ores::database::pq::parse_timestamp_text("2026-10-05 09:00:00+00"));
    CHECK(e.valid_to == db_timestamp::max());
    CHECK(e.oresmd_uri == "oresmd://fx/EUR/USD?type=quote&instrument=fx_spot&quote=rate");
    CHECK(e.observation_datetime ==
          ores::database::pq::parse_timestamp_text("2026-10-05 09:00:00+00"));
}

TEST_CASE("a_short_as_of_row_is_refused", "[repository][as_of]") {
    auto row = observation_columns();
    row.pop_back();

    CHECK_THROWS_WITH(as_of_observation(row, 0, "read_as_of"),
                      ContainsSubstring("read_as_of: a row has 10 columns, expected 11"));
}

TEST_CASE("a_bucket_row_with_no_ordinal_is_refused", "[repository][as_of]") {
    CHECK_THROWS_WITH(as_of_bucket_ordinal(bucket_row(std::nullopt), 4),
                      ContainsSubstring("has no bucket ordinal"));
}

TEST_CASE("a_bucket_ordinal_that_is_not_a_number_is_refused", "[repository][as_of]") {
    CHECK_THROWS_WITH(as_of_bucket_ordinal(bucket_row("two"), 4),
                      ContainsSubstring("'two' is not a number"));
    CHECK_THROWS_WITH(as_of_bucket_ordinal(bucket_row("2x"), 4),
                      ContainsSubstring("'2x' is not a number"));
    CHECK_THROWS_WITH(as_of_bucket_ordinal(bucket_row("-1"), 4),
                      ContainsSubstring("'-1' is not a number"));
    CHECK_THROWS_WITH(as_of_bucket_ordinal(bucket_row(" 1"), 4),
                      ContainsSubstring("' 1' is not a number"));
}

TEST_CASE("a_bucket_ordinal_outside_the_buckets_is_refused", "[repository][as_of]") {
    CHECK(as_of_bucket_ordinal(bucket_row("3"), 4) == 3);
    CHECK_THROWS_WITH(as_of_bucket_ordinal(bucket_row("4"), 4),
                      ContainsSubstring("ordinal 4 is outside the 4 buckets requested"));
}
