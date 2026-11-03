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
#include "ores.synthetic.api/messaging/market_regime_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::get_market_regime_request>::write_json(
    const ores::synthetic::messaging::get_market_regime_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_market_regime_request
wire_traits<ores::synthetic::messaging::get_market_regime_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_market_regime_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_market_regime_request>::write_msgpack(
    const ores::synthetic::messaging::get_market_regime_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_market_regime_request
wire_traits<ores::synthetic::messaging::get_market_regime_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_market_regime_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::market_regime_sample>::write_json(
    const ores::synthetic::messaging::market_regime_sample& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::market_regime_sample
wire_traits<ores::synthetic::messaging::market_regime_sample>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::market_regime_sample>(text);
}

std::string wire_traits<ores::synthetic::messaging::market_regime_sample>::write_msgpack(
    const ores::synthetic::messaging::market_regime_sample& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::market_regime_sample
wire_traits<ores::synthetic::messaging::market_regime_sample>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::market_regime_sample>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_market_regime_response>::write_json(
    const ores::synthetic::messaging::get_market_regime_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_market_regime_response
wire_traits<ores::synthetic::messaging::get_market_regime_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_market_regime_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_market_regime_response>::write_msgpack(
    const ores::synthetic::messaging::get_market_regime_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_market_regime_response
wire_traits<ores::synthetic::messaging::get_market_regime_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_market_regime_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_market_trace_request>::write_json(
    const ores::synthetic::messaging::get_market_trace_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_market_trace_request
wire_traits<ores::synthetic::messaging::get_market_trace_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_market_trace_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_market_trace_request>::write_msgpack(
    const ores::synthetic::messaging::get_market_trace_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_market_trace_request
wire_traits<ores::synthetic::messaging::get_market_trace_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_market_trace_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::market_trace_event>::write_json(
    const ores::synthetic::messaging::market_trace_event& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::market_trace_event
wire_traits<ores::synthetic::messaging::market_trace_event>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::market_trace_event>(text);
}

std::string wire_traits<ores::synthetic::messaging::market_trace_event>::write_msgpack(
    const ores::synthetic::messaging::market_trace_event& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::market_trace_event
wire_traits<ores::synthetic::messaging::market_trace_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::market_trace_event>(data);
}

std::string wire_traits<ores::synthetic::messaging::market_series_state>::write_json(
    const ores::synthetic::messaging::market_series_state& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::market_series_state
wire_traits<ores::synthetic::messaging::market_series_state>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::market_series_state>(text);
}

std::string wire_traits<ores::synthetic::messaging::market_series_state>::write_msgpack(
    const ores::synthetic::messaging::market_series_state& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::market_series_state
wire_traits<ores::synthetic::messaging::market_series_state>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::market_series_state>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_market_trace_response>::write_json(
    const ores::synthetic::messaging::get_market_trace_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_market_trace_response
wire_traits<ores::synthetic::messaging::get_market_trace_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_market_trace_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_market_trace_response>::write_msgpack(
    const ores::synthetic::messaging::get_market_trace_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_market_trace_response
wire_traits<ores::synthetic::messaging::get_market_trace_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_market_trace_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_market_history_request>::write_json(
    const ores::synthetic::messaging::get_market_history_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_market_history_request
wire_traits<ores::synthetic::messaging::get_market_history_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_market_history_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_market_history_request>::write_msgpack(
    const ores::synthetic::messaging::get_market_history_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_market_history_request
wire_traits<ores::synthetic::messaging::get_market_history_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_market_history_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::market_history_bin>::write_json(
    const ores::synthetic::messaging::market_history_bin& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::market_history_bin
wire_traits<ores::synthetic::messaging::market_history_bin>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::market_history_bin>(text);
}

std::string wire_traits<ores::synthetic::messaging::market_history_bin>::write_msgpack(
    const ores::synthetic::messaging::market_history_bin& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::market_history_bin
wire_traits<ores::synthetic::messaging::market_history_bin>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::market_history_bin>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_market_history_response>::write_json(
    const ores::synthetic::messaging::get_market_history_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_market_history_response
wire_traits<ores::synthetic::messaging::get_market_history_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_market_history_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_market_history_response>::write_msgpack(
    const ores::synthetic::messaging::get_market_history_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_market_history_response
wire_traits<ores::synthetic::messaging::get_market_history_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_market_history_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::pin_market_regime_request>::write_json(
    const ores::synthetic::messaging::pin_market_regime_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::pin_market_regime_request
wire_traits<ores::synthetic::messaging::pin_market_regime_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::pin_market_regime_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::pin_market_regime_request>::write_msgpack(
    const ores::synthetic::messaging::pin_market_regime_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::pin_market_regime_request
wire_traits<ores::synthetic::messaging::pin_market_regime_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::pin_market_regime_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::pin_market_regime_response>::write_json(
    const ores::synthetic::messaging::pin_market_regime_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::pin_market_regime_response
wire_traits<ores::synthetic::messaging::pin_market_regime_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::pin_market_regime_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::pin_market_regime_response>::write_msgpack(
    const ores::synthetic::messaging::pin_market_regime_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::pin_market_regime_response
wire_traits<ores::synthetic::messaging::pin_market_regime_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::pin_market_regime_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::configure_market_request>::write_json(
    const ores::synthetic::messaging::configure_market_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::configure_market_request
wire_traits<ores::synthetic::messaging::configure_market_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::configure_market_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::configure_market_request>::write_msgpack(
    const ores::synthetic::messaging::configure_market_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::configure_market_request
wire_traits<ores::synthetic::messaging::configure_market_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::configure_market_request>(data);
}

}
