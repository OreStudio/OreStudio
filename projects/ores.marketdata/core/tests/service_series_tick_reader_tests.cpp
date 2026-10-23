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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/catalogue_ids.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/tick_coords_repository.hpp"
#include "ores.marketdata.core/service/series_tick_reader.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "tick_test_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace {

const std::string tags("[service][ticks]");

using namespace ores::marketdata::generators;
using namespace std::chrono_literals;
using ores::marketdata::messaging::get_series_ticks_request;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::repository::tick_coords_repository;
using ores::marketdata::service::series_tick_reader;
using ores::marketdata::tests::insert_tick;
using ores::marketdata::tests::party_context;
using ores::testing::database_helper;
using ores::utility::domain::outcome;

std::chrono::system_clock::time_point now_second() {
    return std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
}

/// A series that the test owns, with the number of its own datum in the tick dictionary.
struct owned_series {
    ores::marketdata::domain::market_series series;
    ores::database::context owner;
    int number;
};

owned_series make_series(database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);
    auto owner = party_context(h, s.party_id);
    const auto number = tick_coords_repository{}.resolve(owner, s.party_id, s.id, s.oresmd_uri);
    return {std::move(s), std::move(owner), number};
}

get_series_ticks_request span(const owned_series& o,
                              std::chrono::system_clock::time_point from,
                              std::chrono::system_clock::time_point to) {
    get_series_ticks_request request;
    request.series_id = o.series.id;
    request.from_instant = from;
    request.to_instant = to;
    return request;
}

/// Appends @p count ticks of one second apart in one statement, because a statement for each
/// tick would take longer than the test is worth.
void insert_many_ticks(const owned_series& o,
                       std::chrono::system_clock::time_point first,
                       int count) {
    auto lg(ores::logging::make_logger("ores.marketdata.core.tests"));
    ores::database::repository::execute_parameterized_multi_column_query(
        o.owner,
        "WITH ins AS ("
        "  INSERT INTO ores_marketdata_ticks_tbl "
        "    (tenant_id, party_id, series_id, ts, coord, value, source_id) "
        "  SELECT $1::uuid, $2::uuid, $3::uuid, $4::timestamptz + n * interval '1 second', "
        "         $5::smallint, n::float8, $6::uuid "
        "  FROM generate_series(0, $7::int - 1) AS n "
        "  RETURNING 1) "
        "SELECT count(*) FROM ins",
        {o.owner.tenant_id().to_string(),
         boost::uuids::to_string(o.series.party_id),
         boost::uuids::to_string(o.series.id),
         ores::platform::time::datetime::to_iso8601_utc(first),
         std::to_string(o.number),
         boost::uuids::to_string(ores::marketdata::domain::catalogue_ids::ore_import_source_id()),
         std::to_string(count)},
        lg,
        "inserting many ticks");
}

}

TEST_CASE("a_span_returns_every_tick_in_one_reply_with_the_gaps_between_them", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto other = "oresmd://test/" + boost::uuids::to_string(o.series.id) + "/1Y";
    const auto other_number =
        tick_coords_repository{}.resolve(o.owner, o.series.party_id, o.series.id, other);

    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.5);
    insert_tick(o.owner, o.series.party_id, o.series.id, other_number, t0 + 1500000us, 2.5);
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0 + 4s, 3.5);

    const auto reply = series_tick_reader::read(o.owner, span(o, t0 - 1s, t0 + 1min));

    CHECK(reply.result.outcome == outcome::ok);
    REQUIRE(reply.coordinates.size() == 2);
    CHECK(reply.coordinates[0] == o.series.oresmd_uri);
    CHECK(reply.coordinates[1] == other);
    REQUIRE(reply.first_instant.has_value());
    CHECK(*reply.first_instant == t0);
    CHECK(reply.gaps_us == std::vector<std::int64_t>{0, 1500000, 2500000});
    CHECK(reply.coordinate_indexes == std::vector<int>{0, 1, 0});
    CHECK(reply.values == std::vector<double>{1.5, 2.5, 3.5});
}

TEST_CASE("a_span_holds_its_first_instant_and_not_its_last", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0 + 10s, 2.0);
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0 + 20s, 3.0);

    const auto reply = series_tick_reader::read(o.owner, span(o, t0, t0 + 20s));

    CHECK(reply.values == std::vector<double>{1.0, 2.0});
}

TEST_CASE("a_span_with_no_end_reads_up_to_now", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0 + 1h, 2.0);

    auto request = span(o, t0 - 1s, t0);
    request.to_instant.reset();
    const auto reply = series_tick_reader::read(o.owner, request, t0 + 30min);

    CHECK(reply.values == std::vector<double>{1.0});
}

TEST_CASE("a_coordinate_filter_reads_that_coordinate_only", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto other = "oresmd://test/" + boost::uuids::to_string(o.series.id) + "/1Y";
    const auto other_number =
        tick_coords_repository{}.resolve(o.owner, o.series.party_id, o.series.id, other);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);
    insert_tick(o.owner, o.series.party_id, o.series.id, other_number, t0 + 1s, 2.0);

    auto request = span(o, t0 - 1s, t0 + 1min);
    request.coordinates = {other};
    const auto reply = series_tick_reader::read(o.owner, request);

    REQUIRE(reply.coordinates.size() == 1);
    CHECK(reply.coordinates.front() == other);
    CHECK(reply.values == std::vector<double>{2.0});
}

TEST_CASE("a_span_with_no_tick_gives_an_empty_reply_and_success", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();

    const auto reply = series_tick_reader::read(o.owner, span(o, t0, t0 + 1min));

    CHECK(reply.result.outcome == outcome::ok);
    CHECK(reply.coordinates.empty());
    CHECK_FALSE(reply.first_instant.has_value());
    CHECK(reply.gaps_us.empty());
    CHECK(reply.values.empty());
}

TEST_CASE("a_tick_of_another_party_is_not_in_the_reply", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);

    const auto stranger = boost::uuids::random_generator{}();
    const auto reply =
        series_tick_reader::read(party_context(h, stranger), span(o, t0 - 1s, t0 + 1min));

    CHECK(reply.result.outcome == outcome::ok);
    CHECK(reply.values.empty());
}

TEST_CASE("a_session_with_no_party_is_refused_and_not_answered_with_an_empty_reply", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);

    const auto reply = series_tick_reader::read(h.context(), span(o, t0 - 1s, t0 + 1min));

    CHECK(reply.result.outcome == outcome::denied);
    CHECK(reply.result.code == "party_required");
    CHECK(reply.values.empty());
}

TEST_CASE("a_span_that_ends_before_it_starts_is_refused", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();

    const auto reply = series_tick_reader::read(o.owner, span(o, t0, t0 - 1s));

    CHECK(reply.result.outcome == outcome::invalid);
    CHECK(reply.result.code == "range_inverted");
}

TEST_CASE("a_span_that_starts_and_ends_at_one_instant_is_empty_and_not_refused", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);

    const auto reply = series_tick_reader::read(o.owner, span(o, t0, t0));

    CHECK(reply.result.outcome == outcome::ok);
    CHECK(reply.values.empty());
}

TEST_CASE("a_span_at_the_ceiling_is_answered_and_a_span_over_it_is_refused_naming_it", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();
    const auto ceiling = static_cast<int>(series_tick_reader::max_ticks);
    insert_many_ticks(o, t0, ceiling);

    const auto at = series_tick_reader::read(o.owner, span(o, t0, t0 + 24h));
    CHECK(at.result.outcome == outcome::ok);
    CHECK(at.values.size() == series_tick_reader::max_ticks);

    insert_tick(
        o.owner, o.series.party_id, o.series.id, o.number, t0 + std::chrono::seconds(ceiling), 0.0);
    const auto over = series_tick_reader::read(o.owner, span(o, t0, t0 + 24h));
    CHECK(over.result.outcome == outcome::invalid);
    CHECK(over.result.code == "range_too_large");
    CHECK(over.result.message.find("25000") != std::string::npos);
    CHECK(over.values.empty());
}

TEST_CASE("ticks_of_one_instant_come_in_the_order_of_their_coordinates", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto other = "oresmd://test/" + boost::uuids::to_string(o.series.id) + "/1Y";
    const auto other_number =
        tick_coords_repository{}.resolve(o.owner, o.series.party_id, o.series.id, other);
    const auto t0 = now_second();
    insert_tick(o.owner, o.series.party_id, o.series.id, other_number, t0, 2.0);
    insert_tick(o.owner, o.series.party_id, o.series.id, o.number, t0, 1.0);

    const auto reply = series_tick_reader::read(o.owner, span(o, t0 - 1s, t0 + 1min));

    CHECK(reply.coordinates == std::vector<std::string>{o.series.oresmd_uri, other});
    CHECK(reply.values == std::vector<double>{1.0, 2.0});
}

TEST_CASE("a_span_with_no_end_that_starts_after_now_is_empty_and_not_refused", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();

    auto request = span(o, t0 + 1min, t0);
    request.to_instant.reset();
    const auto reply = series_tick_reader::read(o.owner, request, t0);

    CHECK(reply.result.outcome == outcome::ok);
    CHECK(reply.values.empty());
}

TEST_CASE("a_request_that_names_too_many_coordinates_is_refused", tags) {
    database_helper h;
    const auto o = make_series(h);
    const auto t0 = now_second();

    auto request = span(o, t0 - 1s, t0 + 1min);
    for (std::size_t i = 0; i <= series_tick_reader::max_coordinates; ++i)
        request.coordinates.push_back("oresmd://test/" + std::to_string(i));
    const auto reply = series_tick_reader::read(o.owner, request);

    CHECK(reply.result.outcome == outcome::invalid);
    CHECK(reply.result.code == "filter_too_large");
    CHECK(reply.result.message.find("coordinates") != std::string::npos);
}

TEST_CASE("a_reply_of_ticks_and_its_request_survive_the_wire_codec", tags) {
    const auto t0 = now_second();
    get_series_ticks_request request;
    request.series_id = boost::uuids::random_generator{}();
    request.from_instant = t0;
    request.to_instant = std::nullopt;
    request.coordinates = {R"(oresmd://test/a,"b")"};

    const auto& codec = ores::nats::default_wire_codec();
    const auto sent = codec.decode<get_series_ticks_request>(codec.encode(request));
    REQUIRE(sent);
    CHECK(sent->series_id == request.series_id);
    CHECK(sent->from_instant == t0);
    CHECK_FALSE(sent->to_instant.has_value());
    CHECK(sent->coordinates == request.coordinates);

    ores::marketdata::messaging::get_series_ticks_response reply;
    reply.coordinates = {"oresmd://test/a", "oresmd://test/b"};
    reply.first_instant = t0 + 123456us;
    reply.gaps_us = {0, 1500000, 3000000000LL};
    reply.coordinate_indexes = {0, 1, 0};
    reply.values = {1.0845, -0.25, 1e-9};
    const auto received =
        codec.decode<ores::marketdata::messaging::get_series_ticks_response>(codec.encode(reply));
    REQUIRE(received);
    CHECK(received->result.outcome == outcome::ok);
    CHECK(received->coordinates == reply.coordinates);
    REQUIRE(received->first_instant.has_value());
    CHECK(*received->first_instant == *reply.first_instant);
    CHECK(received->gaps_us == reply.gaps_us);
    CHECK(received->coordinate_indexes == reply.coordinate_indexes);
    CHECK(received->values == reply.values);
}
