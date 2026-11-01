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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.api/domain/bond_issue_call_date_codec.hpp"
#include "ores.trading.api/domain/bond_issue_codec.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date_codec.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/structure_codec.hpp"
#include "ores.trading.api/domain/structure_member_codec.hpp"
#include "ores.trading.api/domain/trade_booking_codec.hpp"
#include "ores.trading.api/domain/trade_codec.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_identifier_codec.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::book_trade_request>::write_json(
    const ores::trading::messaging::book_trade_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::book_trade_request
wire_traits<ores::trading::messaging::book_trade_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::book_trade_request>(text);
}

std::string wire_traits<ores::trading::messaging::book_trade_request>::write_msgpack(
    const ores::trading::messaging::book_trade_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::book_trade_request
wire_traits<ores::trading::messaging::book_trade_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::book_trade_request>(data);
}

std::string wire_traits<ores::trading::messaging::book_trade_response>::write_json(
    const ores::trading::messaging::book_trade_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::book_trade_response
wire_traits<ores::trading::messaging::book_trade_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::book_trade_response>(text);
}

std::string wire_traits<ores::trading::messaging::book_trade_response>::write_msgpack(
    const ores::trading::messaging::book_trade_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::book_trade_response
wire_traits<ores::trading::messaging::book_trade_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::book_trade_response>(data);
}

std::string wire_traits<ores::trading::messaging::execute_trade_request>::write_json(
    const ores::trading::messaging::execute_trade_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::execute_trade_request
wire_traits<ores::trading::messaging::execute_trade_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::execute_trade_request>(text);
}

std::string wire_traits<ores::trading::messaging::execute_trade_request>::write_msgpack(
    const ores::trading::messaging::execute_trade_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::execute_trade_request
wire_traits<ores::trading::messaging::execute_trade_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::execute_trade_request>(data);
}

std::string wire_traits<ores::trading::messaging::execute_trade_response>::write_json(
    const ores::trading::messaging::execute_trade_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::execute_trade_response
wire_traits<ores::trading::messaging::execute_trade_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::execute_trade_response>(text);
}

std::string wire_traits<ores::trading::messaging::execute_trade_response>::write_msgpack(
    const ores::trading::messaging::execute_trade_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::execute_trade_response
wire_traits<ores::trading::messaging::execute_trade_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::execute_trade_response>(data);
}

std::string wire_traits<ores::trading::messaging::complete_structure_request>::write_json(
    const ores::trading::messaging::complete_structure_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::complete_structure_request
wire_traits<ores::trading::messaging::complete_structure_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::complete_structure_request>(text);
}

std::string wire_traits<ores::trading::messaging::complete_structure_request>::write_msgpack(
    const ores::trading::messaging::complete_structure_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::complete_structure_request
wire_traits<ores::trading::messaging::complete_structure_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::complete_structure_request>(data);
}

std::string wire_traits<ores::trading::messaging::complete_structure_response>::write_json(
    const ores::trading::messaging::complete_structure_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::complete_structure_response
wire_traits<ores::trading::messaging::complete_structure_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::complete_structure_response>(text);
}

std::string wire_traits<ores::trading::messaging::complete_structure_response>::write_msgpack(
    const ores::trading::messaging::complete_structure_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::complete_structure_response
wire_traits<ores::trading::messaging::complete_structure_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::complete_structure_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_bond_issue_composite_request>::write_json(
    const ores::trading::messaging::put_bond_issue_composite_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_bond_issue_composite_request
wire_traits<ores::trading::messaging::put_bond_issue_composite_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_bond_issue_composite_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_bond_issue_composite_request>::write_msgpack(
    const ores::trading::messaging::put_bond_issue_composite_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_bond_issue_composite_request
wire_traits<ores::trading::messaging::put_bond_issue_composite_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_bond_issue_composite_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_bond_issue_composite_response>::write_json(
    const ores::trading::messaging::put_bond_issue_composite_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_bond_issue_composite_response
wire_traits<ores::trading::messaging::put_bond_issue_composite_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_bond_issue_composite_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_bond_issue_composite_response>::write_msgpack(
    const ores::trading::messaging::put_bond_issue_composite_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_bond_issue_composite_response
wire_traits<ores::trading::messaging::put_bond_issue_composite_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_bond_issue_composite_response>(data);
}

std::string wire_traits<ores::trading::messaging::trade_export_item>::write_json(
    const ores::trading::messaging::trade_export_item& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_export_item
wire_traits<ores::trading::messaging::trade_export_item>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_export_item>(text);
}

std::string wire_traits<ores::trading::messaging::trade_export_item>::write_msgpack(
    const ores::trading::messaging::trade_export_item& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_export_item
wire_traits<ores::trading::messaging::trade_export_item>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_export_item>(data);
}

std::string wire_traits<ores::trading::messaging::export_portfolio_request>::write_json(
    const ores::trading::messaging::export_portfolio_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::export_portfolio_request
wire_traits<ores::trading::messaging::export_portfolio_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::export_portfolio_request>(text);
}

std::string wire_traits<ores::trading::messaging::export_portfolio_request>::write_msgpack(
    const ores::trading::messaging::export_portfolio_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::export_portfolio_request
wire_traits<ores::trading::messaging::export_portfolio_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::export_portfolio_request>(data);
}

std::string wire_traits<ores::trading::messaging::export_portfolio_response>::write_json(
    const ores::trading::messaging::export_portfolio_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::export_portfolio_response
wire_traits<ores::trading::messaging::export_portfolio_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::export_portfolio_response>(text);
}

std::string wire_traits<ores::trading::messaging::export_portfolio_response>::write_msgpack(
    const ores::trading::messaging::export_portfolio_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::export_portfolio_response
wire_traits<ores::trading::messaging::export_portfolio_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::export_portfolio_response>(data);
}

std::string wire_traits<ores::trading::messaging::export_trades_to_storage_request>::write_json(
    const ores::trading::messaging::export_trades_to_storage_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::export_trades_to_storage_request
wire_traits<ores::trading::messaging::export_trades_to_storage_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::export_trades_to_storage_request>(text);
}

std::string wire_traits<ores::trading::messaging::export_trades_to_storage_request>::write_msgpack(
    const ores::trading::messaging::export_trades_to_storage_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::export_trades_to_storage_request
wire_traits<ores::trading::messaging::export_trades_to_storage_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::export_trades_to_storage_request>(data);
}

std::string wire_traits<ores::trading::messaging::export_trades_to_storage_response>::write_json(
    const ores::trading::messaging::export_trades_to_storage_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::export_trades_to_storage_response
wire_traits<ores::trading::messaging::export_trades_to_storage_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::export_trades_to_storage_response>(text);
}

std::string wire_traits<ores::trading::messaging::export_trades_to_storage_response>::write_msgpack(
    const ores::trading::messaging::export_trades_to_storage_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::export_trades_to_storage_response
wire_traits<ores::trading::messaging::export_trades_to_storage_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::export_trades_to_storage_response>(data);
}

}
