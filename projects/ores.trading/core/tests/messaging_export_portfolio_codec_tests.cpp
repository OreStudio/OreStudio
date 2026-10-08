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
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/domain/wire_format.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <variant>
#include <vector>

namespace {

using ores::nats::wire_codec;
using ores::nats::wire_format;
using ores::trading::domain::bond_instrument_data;
using ores::trading::messaging::export_portfolio_response;
using ores::trading::messaging::trade_export_item;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[messaging][codec]");

/**
 * A bond whose identity fields a caller can check, appended to the batch that
 * carries it. Every other member is left default: the point of the case is
 * that the instrument crosses the codec as its own typed array, not how rich
 * the instrument is.
 */
bond_instrument_data make_bond(boost::uuids::uuid trade_id, boost::uuids::uuid issue_id) {
    bond_instrument_data bond;
    bond.instrument.identity.trade_id = trade_id;
    bond.instrument.issue_id = issue_id;
    bond.trs_price_type = "Dirty";
    return bond;
}

std::string check_bond_survives_the_codec(wire_format format) {
    const auto codec = wire_codec{format};
    const auto trade_id = boost::uuids::random_generator()();
    const auto issue_id = boost::uuids::random_generator()();

    export_portfolio_response sent;
    sent.success = true;
    trade_export_item item;
    item.ore_id = "CodecBond001";
    item.anchor.id = trade_id;
    item.anchor.trade_type = "Bond";
    sent.items.push_back(item);
    ores::trading::domain::append_instrument(
        sent.instruments, ores::trading::domain::trade_instrument{make_bond(trade_id, issue_id)});

    const auto bytes = codec.encode(sent);
    const auto decoded = codec.decode<export_portfolio_response>(bytes);
    REQUIRE(decoded.has_value());
    REQUIRE(decoded->items.size() == 1);
    CHECK(decoded->items[0].ore_id == "CodecBond001");

    // The batch's array states its element's type, so a bond the wire dropped
    // would come back absent rather than as an untagged variant. Assert the
    // data the caller observes, not that the codec reported success.
    const auto instrument =
        ores::trading::domain::rebuild_instrument(decoded->instruments, trade_id);
    REQUIRE(std::holds_alternative<bond_instrument_data>(instrument));
    const auto& bond = std::get<bond_instrument_data>(instrument);
    CHECK(bond.instrument.identity.trade_id == trade_id);
    CHECK(bond.instrument.issue_id == issue_id);
    CHECK(bond.trs_price_type == "Dirty");
    return decoded->items[0].ore_id;
}

}

TEST_CASE("export_portfolio_response_survives_the_json_codec", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    CHECK(check_bond_survives_the_codec(wire_format::json) == "CodecBond001");
}

TEST_CASE("export_portfolio_response_survives_the_msgpack_codec", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    CHECK(check_bond_survives_the_codec(wire_format::msgpack) == "CodecBond001");
}

TEST_CASE("export_portfolio_response_carries_a_trade_with_no_instrument", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto codec = wire_codec{wire_format::msgpack};

    export_portfolio_response sent;
    sent.success = true;
    sent.items.push_back(trade_export_item{});
    sent.items[0].ore_id = "NoInstrument001";

    const auto decoded = codec.decode<export_portfolio_response>(codec.encode(sent));
    REQUIRE(decoded.has_value());
    REQUIRE(decoded->items.size() == 1);
    CHECK(decoded->instruments.bond_instruments.empty());
    CHECK(std::holds_alternative<std::monostate>(ores::trading::domain::rebuild_instrument(
        decoded->instruments, decoded->items[0].anchor.id)));
}

TEST_CASE("export_portfolio_response_carries_the_envelope", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto codec = wire_codec{wire_format::json};

    export_portfolio_response sent;
    sent.success = true;
    trade_export_item item;
    item.ore_id = "EnvelopedTrade001";
    ores::trading::domain::trade_envelope_data env;
    env.counter_party = "CPTY";
    env.netting_set_id = "NS-1";
    item.envelope = env;
    sent.items.push_back(item);

    const auto decoded = codec.decode<export_portfolio_response>(codec.encode(sent));
    REQUIRE(decoded.has_value());
    REQUIRE(decoded->items.size() == 1);
    REQUIRE(decoded->items[0].envelope.has_value());
    CHECK(decoded->items[0].envelope->counter_party == "CPTY");
    CHECK(decoded->items[0].envelope->netting_set_id == "NS-1");
}
