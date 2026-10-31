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
#include "ores.refdata.api/domain/calendar_date_codec.hpp"
#include "ores.refdata.api/messaging/calendar_date_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::calendar_date_key>::write_json(
    const ores::refdata::messaging::calendar_date_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::calendar_date_key
wire_traits<ores::refdata::messaging::calendar_date_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::calendar_date_key>(text);
}

std::string wire_traits<ores::refdata::messaging::calendar_date_key>::write_msgpack(
    const ores::refdata::messaging::calendar_date_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::calendar_date_key
wire_traits<ores::refdata::messaging::calendar_date_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::calendar_date_key>(data);
}

std::string wire_traits<ores::refdata::messaging::calendar_date_lookup>::write_json(
    const ores::refdata::messaging::calendar_date_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::calendar_date_lookup
wire_traits<ores::refdata::messaging::calendar_date_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::calendar_date_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::calendar_date_lookup>::write_msgpack(
    const ores::refdata::messaging::calendar_date_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::calendar_date_lookup
wire_traits<ores::refdata::messaging::calendar_date_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::calendar_date_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::calendar_dates_filter>::write_json(
    const ores::refdata::messaging::calendar_dates_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::calendar_dates_filter
wire_traits<ores::refdata::messaging::calendar_dates_filter>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::calendar_dates_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::calendar_dates_filter>::write_msgpack(
    const ores::refdata::messaging::calendar_dates_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::calendar_dates_filter
wire_traits<ores::refdata::messaging::calendar_dates_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::calendar_dates_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::list_calendar_dates_request>::write_json(
    const ores::refdata::messaging::list_calendar_dates_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_calendar_dates_request
wire_traits<ores::refdata::messaging::list_calendar_dates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_calendar_dates_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_calendar_dates_request>::write_msgpack(
    const ores::refdata::messaging::list_calendar_dates_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_calendar_dates_request
wire_traits<ores::refdata::messaging::list_calendar_dates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_calendar_dates_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_calendar_dates_response>::write_json(
    const ores::refdata::messaging::list_calendar_dates_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_calendar_dates_response
wire_traits<ores::refdata::messaging::list_calendar_dates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_calendar_dates_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_calendar_dates_response>::write_msgpack(
    const ores::refdata::messaging::list_calendar_dates_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_calendar_dates_response
wire_traits<ores::refdata::messaging::list_calendar_dates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_calendar_dates_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_date_request>::write_json(
    const ores::refdata::messaging::get_calendar_date_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_calendar_date_request
wire_traits<ores::refdata::messaging::get_calendar_date_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_calendar_date_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_date_request>::write_msgpack(
    const ores::refdata::messaging::get_calendar_date_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_calendar_date_request
wire_traits<ores::refdata::messaging::get_calendar_date_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_calendar_date_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_date_response>::write_json(
    const ores::refdata::messaging::get_calendar_date_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_calendar_date_response
wire_traits<ores::refdata::messaging::get_calendar_date_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_calendar_date_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_date_response>::write_msgpack(
    const ores::refdata::messaging::get_calendar_date_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_calendar_date_response
wire_traits<ores::refdata::messaging::get_calendar_date_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_calendar_date_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_calendar_dates_request>::write_json(
    const ores::refdata::messaging::get_many_calendar_dates_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_calendar_dates_request
wire_traits<ores::refdata::messaging::get_many_calendar_dates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_calendar_dates_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_calendar_dates_request>::write_msgpack(
    const ores::refdata::messaging::get_many_calendar_dates_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_calendar_dates_request
wire_traits<ores::refdata::messaging::get_many_calendar_dates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_calendar_dates_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_calendar_dates_response>::write_json(
    const ores::refdata::messaging::get_many_calendar_dates_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_calendar_dates_response
wire_traits<ores::refdata::messaging::get_many_calendar_dates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_calendar_dates_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_calendar_dates_response>::write_msgpack(
    const ores::refdata::messaging::get_many_calendar_dates_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_calendar_dates_response
wire_traits<ores::refdata::messaging::get_many_calendar_dates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_calendar_dates_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_request>::write_json(
    const ores::refdata::messaging::list_by_calendar_code_calendar_dates_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_by_calendar_code_calendar_dates_request
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_by_calendar_code_calendar_dates_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_request>::write_msgpack(
    const ores::refdata::messaging::list_by_calendar_code_calendar_dates_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_by_calendar_code_calendar_dates_request
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_by_calendar_code_calendar_dates_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_response>::write_json(
    const ores::refdata::messaging::list_by_calendar_code_calendar_dates_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_by_calendar_code_calendar_dates_response
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_by_calendar_code_calendar_dates_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_response>::write_msgpack(
    const ores::refdata::messaging::list_by_calendar_code_calendar_dates_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_by_calendar_code_calendar_dates_response
wire_traits<ores::refdata::messaging::list_by_calendar_code_calendar_dates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_by_calendar_code_calendar_dates_response>(data);
}

}
