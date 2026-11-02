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
#include "ores.reporting.api/domain/report_cashflow_codec.hpp"
#include "ores.reporting.api/messaging/report_cashflow_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::report_cashflow_key>::write_json(
    const ores::reporting::messaging::report_cashflow_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflow_key
wire_traits<ores::reporting::messaging::report_cashflow_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflow_key>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_key>::write_msgpack(
    const ores::reporting::messaging::report_cashflow_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflow_key
wire_traits<ores::reporting::messaging::report_cashflow_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflow_key>(data);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_write>::write_json(
    const ores::reporting::messaging::report_cashflow_write& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflow_write
wire_traits<ores::reporting::messaging::report_cashflow_write>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflow_write>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_write>::write_msgpack(
    const ores::reporting::messaging::report_cashflow_write& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflow_write
wire_traits<ores::reporting::messaging::report_cashflow_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflow_write>(data);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_change>::write_json(
    const ores::reporting::messaging::report_cashflow_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflow_change
wire_traits<ores::reporting::messaging::report_cashflow_change>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflow_change>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_change>::write_msgpack(
    const ores::reporting::messaging::report_cashflow_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflow_change
wire_traits<ores::reporting::messaging::report_cashflow_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflow_change>(data);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_removal>::write_json(
    const ores::reporting::messaging::report_cashflow_removal& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflow_removal
wire_traits<ores::reporting::messaging::report_cashflow_removal>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflow_removal>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_removal>::write_msgpack(
    const ores::reporting::messaging::report_cashflow_removal& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflow_removal
wire_traits<ores::reporting::messaging::report_cashflow_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflow_removal>(data);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_lookup>::write_json(
    const ores::reporting::messaging::report_cashflow_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflow_lookup
wire_traits<ores::reporting::messaging::report_cashflow_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflow_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_lookup>::write_msgpack(
    const ores::reporting::messaging::report_cashflow_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflow_lookup
wire_traits<ores::reporting::messaging::report_cashflow_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflow_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::report_cashflows_filter>::write_json(
    const ores::reporting::messaging::report_cashflows_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflows_filter
wire_traits<ores::reporting::messaging::report_cashflows_filter>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflows_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflows_filter>::write_msgpack(
    const ores::reporting::messaging::report_cashflows_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflows_filter
wire_traits<ores::reporting::messaging::report_cashflows_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflows_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_event>::write_json(
    const ores::reporting::messaging::report_cashflow_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_cashflow_event
wire_traits<ores::reporting::messaging::report_cashflow_event>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_cashflow_event>(text);
}

std::string wire_traits<ores::reporting::messaging::report_cashflow_event>::write_msgpack(
    const ores::reporting::messaging::report_cashflow_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_cashflow_event
wire_traits<ores::reporting::messaging::report_cashflow_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_cashflow_event>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_cashflows_request>::write_json(
    const ores::reporting::messaging::list_report_cashflows_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_cashflows_request
wire_traits<ores::reporting::messaging::list_report_cashflows_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_cashflows_request>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_cashflows_request>::write_msgpack(
    const ores::reporting::messaging::list_report_cashflows_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_cashflows_request
wire_traits<ores::reporting::messaging::list_report_cashflows_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_cashflows_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_cashflows_response>::write_json(
    const ores::reporting::messaging::list_report_cashflows_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_cashflows_response
wire_traits<ores::reporting::messaging::list_report_cashflows_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_cashflows_response>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_cashflows_response>::write_msgpack(
    const ores::reporting::messaging::list_report_cashflows_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_cashflows_response
wire_traits<ores::reporting::messaging::list_report_cashflows_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_cashflows_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_cashflow_request>::write_json(
    const ores::reporting::messaging::get_report_cashflow_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_cashflow_request
wire_traits<ores::reporting::messaging::get_report_cashflow_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_cashflow_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_cashflow_request>::write_msgpack(
    const ores::reporting::messaging::get_report_cashflow_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_cashflow_request
wire_traits<ores::reporting::messaging::get_report_cashflow_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_cashflow_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_cashflow_response>::write_json(
    const ores::reporting::messaging::get_report_cashflow_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_cashflow_response
wire_traits<ores::reporting::messaging::get_report_cashflow_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_cashflow_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_cashflow_response>::write_msgpack(
    const ores::reporting::messaging::get_report_cashflow_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_cashflow_response
wire_traits<ores::reporting::messaging::get_report_cashflow_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_cashflow_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_cashflows_request>::write_json(
    const ores::reporting::messaging::get_many_report_cashflows_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_cashflows_request
wire_traits<ores::reporting::messaging::get_many_report_cashflows_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_cashflows_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_cashflows_request>::write_msgpack(
    const ores::reporting::messaging::get_many_report_cashflows_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_cashflows_request
wire_traits<ores::reporting::messaging::get_many_report_cashflows_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_cashflows_request>(
        data);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_cashflows_response>::write_json(
    const ores::reporting::messaging::get_many_report_cashflows_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_cashflows_response
wire_traits<ores::reporting::messaging::get_many_report_cashflows_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_cashflows_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_cashflows_response>::write_msgpack(
    const ores::reporting::messaging::get_many_report_cashflows_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_cashflows_response
wire_traits<ores::reporting::messaging::get_many_report_cashflows_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_cashflows_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::put_report_cashflow_request>::write_json(
    const ores::reporting::messaging::put_report_cashflow_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_cashflow_request
wire_traits<ores::reporting::messaging::put_report_cashflow_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_cashflow_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_cashflow_request>::write_msgpack(
    const ores::reporting::messaging::put_report_cashflow_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_cashflow_request
wire_traits<ores::reporting::messaging::put_report_cashflow_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_cashflow_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_report_cashflow_response>::write_json(
    const ores::reporting::messaging::put_report_cashflow_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_cashflow_response
wire_traits<ores::reporting::messaging::put_report_cashflow_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_cashflow_response>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_cashflow_response>::write_msgpack(
    const ores::reporting::messaging::put_report_cashflow_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_cashflow_response
wire_traits<ores::reporting::messaging::put_report_cashflow_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_cashflow_response>(data);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_cashflows_request>::write_json(
    const ores::reporting::messaging::put_many_report_cashflows_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_cashflows_request
wire_traits<ores::reporting::messaging::put_many_report_cashflows_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_cashflows_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_cashflows_request>::write_msgpack(
    const ores::reporting::messaging::put_many_report_cashflows_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_cashflows_request
wire_traits<ores::reporting::messaging::put_many_report_cashflows_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_cashflows_request>(
        data);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_cashflows_response>::write_json(
    const ores::reporting::messaging::put_many_report_cashflows_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_cashflows_response
wire_traits<ores::reporting::messaging::put_many_report_cashflows_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_cashflows_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_cashflows_response>::write_msgpack(
    const ores::reporting::messaging::put_many_report_cashflows_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_cashflows_response
wire_traits<ores::reporting::messaging::put_many_report_cashflows_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_cashflows_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_cashflow_request>::write_json(
    const ores::reporting::messaging::delete_report_cashflow_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_cashflow_request
wire_traits<ores::reporting::messaging::delete_report_cashflow_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_cashflow_request>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_report_cashflow_request>::write_msgpack(
    const ores::reporting::messaging::delete_report_cashflow_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_cashflow_request
wire_traits<ores::reporting::messaging::delete_report_cashflow_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_cashflow_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_cashflow_response>::write_json(
    const ores::reporting::messaging::delete_report_cashflow_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_cashflow_response
wire_traits<ores::reporting::messaging::delete_report_cashflow_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_cashflow_response>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_report_cashflow_response>::write_msgpack(
    const ores::reporting::messaging::delete_report_cashflow_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_cashflow_response
wire_traits<ores::reporting::messaging::delete_report_cashflow_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_cashflow_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_request>::write_json(
    const ores::reporting::messaging::delete_many_report_cashflows_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_cashflows_request
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_cashflows_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_request>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_cashflows_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_cashflows_request
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_cashflows_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_response>::write_json(
    const ores::reporting::messaging::delete_many_report_cashflows_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_cashflows_response
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_cashflows_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_response>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_cashflows_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_cashflows_response
wire_traits<ores::reporting::messaging::delete_many_report_cashflows_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_cashflows_response>(
        data);
}

}
