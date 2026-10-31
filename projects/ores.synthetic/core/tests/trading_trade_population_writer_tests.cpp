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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/fx_instruments.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

using ores::synthetic::domain::trade_generation_scope;
using ores::synthetic::domain::trade_population_slot;
using ores::synthetic::service::trading_trade_population_writer;

trade_population_slot a_slot() {
    boost::uuids::random_generator generator;
    trade_population_slot slot;
    slot.trade_id = generator();
    slot.book_id = generator();
    slot.counterparty_id = generator();
    slot.trader.username = "diane.kay";
    slot.trade_type_code = "COMMFWD";
    slot.internal_identifier = "POP-000001";
    slot.trade_date = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{10}, std::chrono::day{31}};
    slot.instruments.commodity_instruments.push_back(ores::trading::domain::commodity_instrument{});
    return slot;
}

trade_generation_scope a_scope() {
    boost::uuids::random_generator generator;
    trade_generation_scope scope;
    scope.config_id = generator();
    scope.party_id = generator();
    return scope;
}

}

TEST_CASE("a_generated_trade_books_without_asking_for_authorisation", "[writer]") {
    const auto request = trading_trade_population_writer::book_request_for(a_slot(), a_scope());

    CHECK(request.anchor.booking_nature == ores::trading::domain::booking_nature::test);
}

TEST_CASE("a_generated_trade_states_the_person_it_was_entered_by_and_its_own_origin", "[writer]") {
    const auto slot = a_slot();
    const auto scope = a_scope();

    const auto request = trading_trade_population_writer::book_request_for(slot, scope);

    CHECK(request.anchor.id == slot.trade_id);
    CHECK(request.anchor.party_id == scope.party_id);
    CHECK(request.anchor.origin == ores::trading::domain::origin::synthetic);
    CHECK(request.anchor.origin_reference == boost::uuids::to_string(scope.config_id));
    CHECK(request.anchor.modified_by == slot.trader.username);
    CHECK(request.booking.book_id == slot.book_id);
    CHECK(request.identifiers.size() == 1);
    CHECK(request.identifiers.front().id_value == slot.internal_identifier);
}

TEST_CASE("a_generated_trade_states_the_rows_its_slot_carries", "[writer]") {
    const auto request = trading_trade_population_writer::book_request_for(a_slot(), a_scope());

    CHECK(request.instruments.commodity_instruments.size() == 1);
}

/*
 * The batch travels to the trading service as the operation's own domain type,
 * so a date that arrives changed is a wire defect and not a planning one. The
 * variance swap's table states that its end falls after its start, which is the
 * only place in the population that can catch a date the codec lost.
 */
TEST_CASE("a_wire_round_trip_keeps_a_variance_swaps_dates", "[writer]") {
    using ores::trading::domain::fx_variance_swap_terms;

    const auto start = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{10}, std::chrono::day{31}};
    const auto end = std::chrono::year_month_day{
        std::chrono::year{2027}, std::chrono::month{10}, std::chrono::day{31}};
    fx_variance_swap_terms terms{start, end, "USD", "AAPL", "Long", 16.0, {}, "Variance"};
    terms.notional = ores::trading::domain::decimal::from_double(1000.0).value();

    ores::trading::messaging::book_trade_request request;
    request.instruments.fx_variance_swap_instruments.push_back(
        ores::trading::domain::construct_fx_variance_swap(terms));

    const auto& codec = ores::nats::default_wire_codec();
    const auto bytes = codec.encode(request);
    const auto decoded = codec.decode<ores::trading::messaging::book_trade_request>(bytes);
    REQUIRE(decoded.has_value());
    REQUIRE(decoded->instruments.fx_variance_swap_instruments.size() == 1);

    const auto& row = decoded->instruments.fx_variance_swap_instruments.front();
    CHECK(row.start_date == start);
    CHECK(row.end_date == end);
}
