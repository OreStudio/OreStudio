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
#include "ores.trading.api/domain/trade_summary_codec.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::trade_summary_key>::write_json(
    const ores::trading::messaging::trade_summary_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_summary_key
wire_traits<ores::trading::messaging::trade_summary_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_summary_key>(text);
}

std::string wire_traits<ores::trading::messaging::trade_summary_key>::write_msgpack(
    const ores::trading::messaging::trade_summary_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_summary_key
wire_traits<ores::trading::messaging::trade_summary_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_summary_key>(data);
}

std::string wire_traits<ores::trading::messaging::trade_summary_lookup>::write_json(
    const ores::trading::messaging::trade_summary_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_summary_lookup
wire_traits<ores::trading::messaging::trade_summary_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_summary_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::trade_summary_lookup>::write_msgpack(
    const ores::trading::messaging::trade_summary_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_summary_lookup
wire_traits<ores::trading::messaging::trade_summary_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_summary_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::trade_summaries_filter>::write_json(
    const ores::trading::messaging::trade_summaries_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_summaries_filter
wire_traits<ores::trading::messaging::trade_summaries_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_summaries_filter>(text);
}

std::string wire_traits<ores::trading::messaging::trade_summaries_filter>::write_msgpack(
    const ores::trading::messaging::trade_summaries_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_summaries_filter
wire_traits<ores::trading::messaging::trade_summaries_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_summaries_filter>(data);
}

std::string wire_traits<ores::trading::messaging::trade_summary_event>::write_json(
    const ores::trading::messaging::trade_summary_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_summary_event
wire_traits<ores::trading::messaging::trade_summary_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_summary_event>(text);
}

std::string wire_traits<ores::trading::messaging::trade_summary_event>::write_msgpack(
    const ores::trading::messaging::trade_summary_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_summary_event
wire_traits<ores::trading::messaging::trade_summary_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_summary_event>(data);
}

std::string wire_traits<ores::trading::messaging::list_trade_summaries_request>::write_json(
    const ores::trading::messaging::list_trade_summaries_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_summaries_request
wire_traits<ores::trading::messaging::list_trade_summaries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_summaries_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_trade_summaries_request>::write_msgpack(
    const ores::trading::messaging::list_trade_summaries_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_summaries_request
wire_traits<ores::trading::messaging::list_trade_summaries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_summaries_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_trade_summaries_response>::write_json(
    const ores::trading::messaging::list_trade_summaries_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_summaries_response
wire_traits<ores::trading::messaging::list_trade_summaries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_summaries_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_trade_summaries_response>::write_msgpack(
    const ores::trading::messaging::list_trade_summaries_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_summaries_response
wire_traits<ores::trading::messaging::list_trade_summaries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_summaries_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_summary_request>::write_json(
    const ores::trading::messaging::get_trade_summary_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_summary_request
wire_traits<ores::trading::messaging::get_trade_summary_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_summary_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_summary_request>::write_msgpack(
    const ores::trading::messaging::get_trade_summary_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_summary_request
wire_traits<ores::trading::messaging::get_trade_summary_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_summary_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_summary_response>::write_json(
    const ores::trading::messaging::get_trade_summary_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_summary_response
wire_traits<ores::trading::messaging::get_trade_summary_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_summary_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_summary_response>::write_msgpack(
    const ores::trading::messaging::get_trade_summary_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_summary_response
wire_traits<ores::trading::messaging::get_trade_summary_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_summary_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_summaries_request>::write_json(
    const ores::trading::messaging::get_many_trade_summaries_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_trade_summaries_request
wire_traits<ores::trading::messaging::get_many_trade_summaries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_trade_summaries_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_summaries_request>::write_msgpack(
    const ores::trading::messaging::get_many_trade_summaries_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_trade_summaries_request
wire_traits<ores::trading::messaging::get_many_trade_summaries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_trade_summaries_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_summaries_response>::write_json(
    const ores::trading::messaging::get_many_trade_summaries_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_trade_summaries_response
wire_traits<ores::trading::messaging::get_many_trade_summaries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_trade_summaries_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_summaries_response>::write_msgpack(
    const ores::trading::messaging::get_many_trade_summaries_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_trade_summaries_response
wire_traits<ores::trading::messaging::get_many_trade_summaries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_trade_summaries_response>(data);
}

}
