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
#include "ores.refdata.api/messaging/calendar_adjustment_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::calendar_adjustment>::write_json(
    const ores::refdata::messaging::calendar_adjustment& v) {
    return object::write_json(v);
}

ores::refdata::messaging::calendar_adjustment
wire_traits<ores::refdata::messaging::calendar_adjustment>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::calendar_adjustment>(text);
}

std::string wire_traits<ores::refdata::messaging::calendar_adjustment>::write_msgpack(
    const ores::refdata::messaging::calendar_adjustment& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::calendar_adjustment
wire_traits<ores::refdata::messaging::calendar_adjustment>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::calendar_adjustment>(data);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_adjustments_request>::write_json(
    const ores::refdata::messaging::get_calendar_adjustments_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_calendar_adjustments_request
wire_traits<ores::refdata::messaging::get_calendar_adjustments_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_calendar_adjustments_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_adjustments_request>::write_msgpack(
    const ores::refdata::messaging::get_calendar_adjustments_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_calendar_adjustments_request
wire_traits<ores::refdata::messaging::get_calendar_adjustments_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_calendar_adjustments_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_adjustments_response>::write_json(
    const ores::refdata::messaging::get_calendar_adjustments_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_calendar_adjustments_response
wire_traits<ores::refdata::messaging::get_calendar_adjustments_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_calendar_adjustments_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_calendar_adjustments_response>::write_msgpack(
    const ores::refdata::messaging::get_calendar_adjustments_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_calendar_adjustments_response
wire_traits<ores::refdata::messaging::get_calendar_adjustments_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_calendar_adjustments_response>(data);
}

}
