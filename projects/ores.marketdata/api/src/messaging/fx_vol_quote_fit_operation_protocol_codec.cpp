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
#include "ores.marketdata.api/messaging/fx_vol_quote_fit_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::fx_vol_quote>::write_json(
    const ores::marketdata::messaging::fx_vol_quote& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_quote
wire_traits<ores::marketdata::messaging::fx_vol_quote>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_quote>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_quote>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_quote& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_quote
wire_traits<ores::marketdata::messaging::fx_vol_quote>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_quote>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_quote_source>::write_json(
    const ores::marketdata::messaging::fx_vol_quote_source& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_quote_source
wire_traits<ores::marketdata::messaging::fx_vol_quote_source>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_quote_source>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_quote_source>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_quote_source& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_quote_source
wire_traits<ores::marketdata::messaging::fx_vol_quote_source>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_quote_source>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_delta_point>::write_json(
    const ores::marketdata::messaging::fx_vol_delta_point& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_delta_point
wire_traits<ores::marketdata::messaging::fx_vol_delta_point>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_delta_point>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_delta_point>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_delta_point& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_delta_point
wire_traits<ores::marketdata::messaging::fx_vol_delta_point>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_delta_point>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_source_fit>::write_json(
    const ores::marketdata::messaging::fx_vol_source_fit& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_source_fit
wire_traits<ores::marketdata::messaging::fx_vol_source_fit>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_source_fit>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_source_fit>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_source_fit& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_source_fit
wire_traits<ores::marketdata::messaging::fx_vol_source_fit>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_source_fit>(data);
}

std::string wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_request>::write_json(
    const ores::marketdata::messaging::fit_fx_vol_quotes_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fit_fx_vol_quotes_request
wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fit_fx_vol_quotes_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_request>::write_msgpack(
    const ores::marketdata::messaging::fit_fx_vol_quotes_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fit_fx_vol_quotes_request
wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fit_fx_vol_quotes_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_response>::write_json(
    const ores::marketdata::messaging::fit_fx_vol_quotes_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fit_fx_vol_quotes_response
wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fit_fx_vol_quotes_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_response>::write_msgpack(
    const ores::marketdata::messaging::fit_fx_vol_quotes_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fit_fx_vol_quotes_response
wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fit_fx_vol_quotes_response>(data);
}

}
