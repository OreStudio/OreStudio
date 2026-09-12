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
#include "ores.trading.api/domain/instrument_payload.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
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
 * A bond export item carrying the identity fields a caller can check. Every
 * other member is left default: the point of the case is which alternative
 * survives the codec, not how rich the instrument is.
 */
trade_export_item make_bond_item(boost::uuids::uuid instrument_id, boost::uuids::uuid issue_id) {
    bond_instrument_data bond;
    bond.instrument.identity.instrument_id = instrument_id;
    bond.instrument.issue_id = issue_id;
    bond.trs_price_type = "Dirty";

    trade_export_item item;
    item.trade.identity.external_id = "CodecBond001";
    item.trade.classification.trade_type = "Bond";
    item.instrument = ores::trading::domain::encode_instrument(
        ores::trading::domain::trade_instrument{bond});
    return item;
}

void check_bond_survives_the_codec(wire_format format) {
    const auto codec = wire_codec{format};
    const auto instrument_id = boost::uuids::random_generator()();
    const auto issue_id = boost::uuids::random_generator()();

    export_portfolio_response sent;
    sent.success = true;
    sent.items.push_back(make_bond_item(instrument_id, issue_id));

    const auto bytes = codec.encode(sent);
    const auto decoded = codec.decode<export_portfolio_response>(bytes);
    REQUIRE(decoded.has_value());
    REQUIRE(decoded->items.size() == 1);
    CHECK(decoded->items[0].trade.identity.external_id == "CodecBond001");

    // An untagged variant decodes as its first alternative, so a bond that
    // comes back as monostate is a silent loss, not an error: the codec
    // reports success either way. Assert the data, not the success.
    const auto instrument =
        ores::trading::domain::decode_instrument(decoded->items[0].instrument);
    REQUIRE(std::holds_alternative<bond_instrument_data>(instrument));
    const auto& bond = std::get<bond_instrument_data>(instrument);
    CHECK(bond.instrument.identity.instrument_id == instrument_id);
    CHECK(bond.instrument.issue_id == issue_id);
    CHECK(bond.trs_price_type == "Dirty");
}

}

TEST_CASE("export_portfolio_response_survives_the_json_codec", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    check_bond_survives_the_codec(wire_format::json);
}

TEST_CASE("export_portfolio_response_survives_the_msgpack_codec", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    check_bond_survives_the_codec(wire_format::msgpack);
}

TEST_CASE("export_portfolio_response_carries_an_absent_instrument", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto codec = wire_codec{wire_format::msgpack};

    export_portfolio_response sent;
    sent.success = true;
    sent.items.push_back(trade_export_item{});
    sent.items[0].trade.identity.external_id = "NoInstrument001";

    const auto decoded = codec.decode<export_portfolio_response>(codec.encode(sent));
    REQUIRE(decoded.has_value());
    REQUIRE(decoded->items.size() == 1);
    CHECK(decoded->items[0].instrument.type.empty());
    CHECK(std::holds_alternative<std::monostate>(
        ores::trading::domain::decode_instrument(decoded->items[0].instrument)));
}

TEST_CASE("export_portfolio_response_carries_the_envelope", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto codec = wire_codec{wire_format::json};

    export_portfolio_response sent;
    sent.success = true;
    trade_export_item item;
    item.trade.identity.external_id = "EnvelopedTrade001";
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
