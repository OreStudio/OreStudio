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
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_payload.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <variant>

namespace {

using ores::trading::domain::bond_instrument_data;
using ores::trading::domain::decode_instrument;
using ores::trading::domain::encode_instrument;
using ores::trading::domain::instrument_payload;
using ores::trading::domain::trade_instrument;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

/**
 * Every alternative must come back as itself. std::monostate is the first
 * alternative, and an untagged variant decodes as monostate, so the index is
 * the thing worth asserting: a decode that succeeds but lands on the wrong
 * alternative is a silent loss.
 */
template <std::size_t I>
void check_alternative_survives() {
    using Alternative = std::variant_alternative_t<I, trade_instrument>;
    const trade_instrument original{Alternative{}};
    const auto decoded = decode_instrument(encode_instrument(original));
    CHECK(decoded.index() == I);
}

}

TEST_CASE("instrument_payload_round_trips_every_alternative", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // A new instrument family must fail the build here, not pass silently
    // through the list below.
    static_assert(std::variant_size_v<trade_instrument> == 9);
    check_alternative_survives<0>();
    check_alternative_survives<1>();
    check_alternative_survives<2>();
    check_alternative_survives<3>();
    check_alternative_survives<4>();
    check_alternative_survives<5>();
    check_alternative_survives<6>();
    check_alternative_survives<7>();
    check_alternative_survives<8>();
}

TEST_CASE("instrument_payload_carries_the_leaf_data", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto instrument_id = boost::uuids::random_generator()();
    const auto issue_id = boost::uuids::random_generator()();

    bond_instrument_data bond;
    bond.instrument.identity.instrument_id = instrument_id;
    bond.instrument.issue_id = issue_id;

    const auto payload = encode_instrument(trade_instrument{bond});
    CHECK(payload.type == "bond_instrument_data");
    CHECK(!payload.body.empty());

    const auto decoded = decode_instrument(payload);
    REQUIRE(std::holds_alternative<bond_instrument_data>(decoded));
    const auto& round_tripped = std::get<bond_instrument_data>(decoded);
    CHECK(round_tripped.instrument.identity.instrument_id == instrument_id);
    CHECK(round_tripped.instrument.issue_id == issue_id);
}

TEST_CASE("instrument_payload_encodes_absence_as_empty", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto payload = encode_instrument(trade_instrument{std::monostate{}});
    CHECK(payload.type.empty());
    CHECK(payload.body.empty());
    CHECK(std::holds_alternative<std::monostate>(decode_instrument(payload)));
}

TEST_CASE("instrument_payload_refuses_an_unrecognised_type", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto decoded =
        decode_instrument(instrument_payload{.type = "not_an_instrument", .body = "{}"});
    CHECK(std::holds_alternative<std::monostate>(decoded));
}

TEST_CASE("instrument_payload_refuses_a_body_that_does_not_parse", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto decoded =
        decode_instrument(instrument_payload{.type = "bond_instrument_data", .body = "not json"});
    CHECK(std::holds_alternative<std::monostate>(decoded));
}
