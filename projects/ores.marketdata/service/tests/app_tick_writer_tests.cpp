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
#include "ores.marketdata.service/app/tick_writer.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::marketdata::service::app::tick_copy_line;
using ores::marketdata::service::app::tick_row;
using ores::marketdata::service::app::tick_writer;
using ores::marketdata::service::app::tick_writer_options;

tick_row sample_row() {
    using namespace std::chrono;
    tick_row row;
    row.tenant_id = "11111111-1111-1111-1111-111111111111";
    row.party_id = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa";
    row.series_id = "cccccccc-cccc-cccc-cccc-cccccccccccc";
    row.ts = sys_days{2026y / October / 21} + 10h + 30min + 5s + 123456us;
    row.value = 1.0845;
    row.source_id = "dddddddd-dddd-dddd-dddd-dddddddddddd";
    return row;
}

}

TEST_CASE("a_tick_row_is_one_csv_line_in_the_column_order_of_the_table", "[app][tick_writer]") {
    CHECK(tick_copy_line(sample_row()) ==
          "11111111-1111-1111-1111-111111111111,aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa,"
          "cccccccc-cccc-cccc-cccc-cccccccccccc,2026-10-21 10:30:05.123456+00,0,1.0845,"
          "dddddddd-dddd-dddd-dddd-dddddddddddd\n");
}

TEST_CASE("a_tick_time_keeps_microseconds_and_drops_finer_digits", "[app][tick_writer]") {
    using namespace std::chrono;
    auto row = sample_row();
    row.ts += 789ns;
    CHECK(tick_copy_line(row).find("2026-10-21 10:30:05.123456+00,") != std::string::npos);
}

TEST_CASE("a_value_that_does_not_round_trip_in_fewer_digits_is_written_in_full", "[app][tick_writer]") {
    auto row = sample_row();
    row.value = 0.1 + 0.2;
    CHECK(tick_copy_line(row).find(",0.30000000000000004,") != std::string::npos);
}

TEST_CASE("a_write_to_a_database_that_is_not_there_throws_and_the_writer_stays_usable", "[app][tick_writer]") {
    tick_writer writer(tick_writer_options{
        .user = "nobody", .password = "none", .host = "127.0.0.1", .database = "none", .port = 1});
    const std::vector<tick_row> rows{sample_row()};
    CHECK_THROWS_AS(writer.write(rows), std::runtime_error);
    CHECK_THROWS_AS(writer.write(rows), std::runtime_error);
    CHECK_NOTHROW(writer.write({}));
}
