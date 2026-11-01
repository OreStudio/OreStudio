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
#include "ores.trading.api/domain/product_type.hpp"
#include "ores.trading.api/domain/product_type_codec.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_codec.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/repository/product_type_entity.hpp"
#include "ores.trading.core/repository/product_type_mapper.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <string>
#include <vector>

namespace {

/**
 * @brief The bytes reflect-cpp writes for @p v.
 */
template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> message_bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief Asserts that one type's codec writes what reflect-cpp writes, in both
 * formats, and reads those bytes back to the same bytes.
 *
 * The generated codec is the one the trait resolves to, so this compares the
 * bytes this component now sends against the bytes it sent before the move,
 * which is the trade's acceptance criterion stated as a test.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(message_bytes_of(msgpack))) == msgpack);

    // The specialisation is the generated one, not the reflector-backed primary
    // template: a translation unit that includes the codec resolves the seam to
    // generated code.
    CHECK(codec::is_generated);
}

/**
 * @brief A trade whose every member that can carry a value does, and whose four
 * enumerations are not their first enumerator.
 *
 * The enumerations are the point: they cross the wire as their names, and the
 * repository mapper spells them the same way, so a case that left them at their
 * defaults would not exercise the rule at all.
 */
ores::trading::domain::trade a_trade() {
    ores::trading::domain::trade t;
    t.version = 3;
    t.tenant_id =
        ores::utility::uuid::tenant_id::from_string("8f0e5d3a-1c2b-4a59-8b7e-2d1f0c9a4b31").value();
    t.id = boost::uuids::string_generator()("033b4053-fe1d-5901-bc2f-7d95afe91f00");
    t.party_id = boost::uuids::string_generator()("ff1c9c31-1b6e-4f36-8d60-2b0b1a7e5d11");
    t.counterparty_id = boost::uuids::string_generator()("a2b3c4d5-e6f7-4a8b-9c0d-1e2f3a4b5c6d");
    t.trade_type = "Swap";
    t.counterparty_scope = ores::trading::domain::counterparty_scope::inter_entity;
    t.booking_nature = ores::trading::domain::booking_nature::hypothetical;
    t.entry_channel = ores::trading::domain::entry_channel::stp;
    t.origin = ores::trading::domain::origin::ore_import;
    t.origin_reference = "ORE-2026-0001";
    t.external_version = 7;
    t.economic_digest = "3f9a";
    t.modified_by = "admin";
    t.performed_by = "admin";
    t.change_reason_code = "system.test";
    t.change_commentary = "Synthetic test data";
    t.recorded_at =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    return t;
}

ores::trading::domain::product_type a_product_type() {
    ores::trading::domain::product_type pt;
    pt.version = 2;
    pt.code = "Swap";
    pt.name = "Interest rate swap";
    pt.description = "Vanilla interest rate swap";
    pt.source = "ore";
    pt.source_ref = "Swap";
    pt.product_family_code = "swap";
    pt.construction = "native";
    pt.has_options = true;
    pt.has_extension = false;
    pt.instrument_table = "ores_trading_vanilla_swap_instruments_tbl";
    pt.display_order = 7;
    pt.knowledge_note = "The plain trades.";
    pt.modified_by = "admin";
    pt.performed_by = "admin";
    pt.change_reason_code = "system.test";
    pt.change_commentary = "Synthetic test data";
    pt.recorded_at =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    return pt;
}

/**
 * @brief An export with one trade, a full envelope and two instrument arrays.
 *
 * The export is stored as a blob that ores.ore reads with reflect-cpp, so the
 * generated codec has to write exactly the reflector's bytes for it.
 */
ores::trading::messaging::export_portfolio_response an_export() {
    ores::trading::messaging::export_portfolio_response r;
    r.success = true;
    r.message = "Exported 1 trades.";
    ores::trading::messaging::trade_export_item item;
    item.anchor = a_trade();
    item.ore_id = "TRADE-1";
    ores::trading::domain::trade_envelope_data envelope;
    envelope.counter_party = "CPTY_A";
    envelope.netting_set_id = "NS-1";
    envelope.portfolio_ids = std::vector<std::string>{"P1", "P2"};
    envelope.additional_fields =
        std::vector<ores::trading::domain::trade_envelope_field>{{"desk", "rates"}};
    item.envelope = envelope;
    r.items.push_back(item);
    r.instruments.rate_instruments.emplace_back();
    r.instruments.bond_issues.emplace_back();
    return r;
}

}

TEST_CASE("a_trade_writes_the_bytes_the_reflector_wrote", "[messaging][codec]") {
    auto lg(ores::logging::make_logger(test_suite));

    same_bytes_as_reflect_cpp(a_trade());
}

TEST_CASE("a_product_type_writes_the_bytes_the_reflector_wrote", "[messaging][codec]") {
    auto lg(ores::logging::make_logger(test_suite));

    same_bytes_as_reflect_cpp(a_product_type());
}

TEST_CASE("an_export_writes_the_bytes_the_reflector_wrote", "[messaging][codec]") {
    auto lg(ores::logging::make_logger(test_suite));

    same_bytes_as_reflect_cpp(an_export());
}

TEST_CASE("the_mapper_carries_a_product_type_through_its_columns", "[repository][codec]") {
    auto lg(ores::logging::make_logger(test_suite));

    // A product type holds text in every column that names something: its code,
    // the family and construction it reads, and what it points at. The store
    // takes each one as written.
    const auto pt = a_product_type();
    const auto entity = ores::trading::repository::product_type_mapper::map(pt);
    CHECK(entity.code == "Swap");
    CHECK(entity.product_family_code == std::optional<std::string>("swap"));
    CHECK(entity.construction == "native");
    CHECK(entity.instrument_table ==
          std::optional<std::string>("ores_trading_vanilla_swap_instruments_tbl"));

    // Reading the store back gives the same values, which is what makes the
    // mapping a round trip rather than a one-way choice.
    const auto restored = ores::trading::repository::product_type_mapper::map(entity);
    CHECK(restored.code == pt.code);
    CHECK(restored.product_family_code == pt.product_family_code);
    CHECK(restored.construction == pt.construction);
    CHECK(restored.has_options == pt.has_options);
    CHECK(restored.display_order == pt.display_order);

    // A text column the type leaves out is absent from the store, and reads back
    // as the empty string the type states.
    auto unstated = a_product_type();
    unstated.product_family_code.clear();
    const auto unstated_entity = ores::trading::repository::product_type_mapper::map(unstated);
    CHECK(unstated_entity.product_family_code == std::nullopt);
    CHECK(ores::trading::repository::product_type_mapper::map(unstated_entity)
              .product_family_code.empty());
}
