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
#include "ores.marketdata.api/messaging/billing_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::bill_line_view>::write_json(
    const ores::marketdata::messaging::bill_line_view& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::bill_line_view
wire_traits<ores::marketdata::messaging::bill_line_view>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::bill_line_view>(text);
}

std::string wire_traits<ores::marketdata::messaging::bill_line_view>::write_msgpack(
    const ores::marketdata::messaging::bill_line_view& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::bill_line_view
wire_traits<ores::marketdata::messaging::bill_line_view>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::bill_line_view>(data);
}

std::string wire_traits<ores::marketdata::messaging::source_bill_view>::write_json(
    const ores::marketdata::messaging::source_bill_view& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::source_bill_view
wire_traits<ores::marketdata::messaging::source_bill_view>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::source_bill_view>(text);
}

std::string wire_traits<ores::marketdata::messaging::source_bill_view>::write_msgpack(
    const ores::marketdata::messaging::source_bill_view& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::source_bill_view
wire_traits<ores::marketdata::messaging::source_bill_view>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::source_bill_view>(data);
}

std::string wire_traits<ores::marketdata::messaging::party_charge_view>::write_json(
    const ores::marketdata::messaging::party_charge_view& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::party_charge_view
wire_traits<ores::marketdata::messaging::party_charge_view>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::party_charge_view>(text);
}

std::string wire_traits<ores::marketdata::messaging::party_charge_view>::write_msgpack(
    const ores::marketdata::messaging::party_charge_view& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::party_charge_view
wire_traits<ores::marketdata::messaging::party_charge_view>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::party_charge_view>(data);
}

std::string wire_traits<ores::marketdata::messaging::compute_bill_request>::write_json(
    const ores::marketdata::messaging::compute_bill_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::compute_bill_request
wire_traits<ores::marketdata::messaging::compute_bill_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::compute_bill_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::compute_bill_request>::write_msgpack(
    const ores::marketdata::messaging::compute_bill_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::compute_bill_request
wire_traits<ores::marketdata::messaging::compute_bill_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::compute_bill_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::compute_bill_response>::write_json(
    const ores::marketdata::messaging::compute_bill_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::compute_bill_response
wire_traits<ores::marketdata::messaging::compute_bill_response>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::compute_bill_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::compute_bill_response>::write_msgpack(
    const ores::marketdata::messaging::compute_bill_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::compute_bill_response
wire_traits<ores::marketdata::messaging::compute_bill_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::compute_bill_response>(data);
}

}
