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
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::published_curve_point>::write_json(
    const ores::marketdata::messaging::published_curve_point& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::published_curve_point
wire_traits<ores::marketdata::messaging::published_curve_point>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::published_curve_point>(text);
}

std::string wire_traits<ores::marketdata::messaging::published_curve_point>::write_msgpack(
    const ores::marketdata::messaging::published_curve_point& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::published_curve_point
wire_traits<ores::marketdata::messaging::published_curve_point>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::published_curve_point>(data);
}

std::string wire_traits<ores::marketdata::messaging::published_curve_set>::write_json(
    const ores::marketdata::messaging::published_curve_set& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::published_curve_set
wire_traits<ores::marketdata::messaging::published_curve_set>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::published_curve_set>(text);
}

std::string wire_traits<ores::marketdata::messaging::published_curve_set>::write_msgpack(
    const ores::marketdata::messaging::published_curve_set& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::published_curve_set
wire_traits<ores::marketdata::messaging::published_curve_set>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::published_curve_set>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_published_curves_request>::write_json(
    const ores::marketdata::messaging::list_published_curves_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_published_curves_request
wire_traits<ores::marketdata::messaging::list_published_curves_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_published_curves_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_published_curves_request>::write_msgpack(
    const ores::marketdata::messaging::list_published_curves_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_published_curves_request
wire_traits<ores::marketdata::messaging::list_published_curves_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_published_curves_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_published_curves_response>::write_json(
    const ores::marketdata::messaging::list_published_curves_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_published_curves_response
wire_traits<ores::marketdata::messaging::list_published_curves_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_published_curves_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_published_curves_response>::write_msgpack(
    const ores::marketdata::messaging::list_published_curves_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_published_curves_response
wire_traits<ores::marketdata::messaging::list_published_curves_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_published_curves_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_published_curve_sets_request>::write_json(
    const ores::marketdata::messaging::get_published_curve_sets_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_published_curve_sets_request
wire_traits<ores::marketdata::messaging::get_published_curve_sets_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_published_curve_sets_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_published_curve_sets_request>::write_msgpack(
    const ores::marketdata::messaging::get_published_curve_sets_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_published_curve_sets_request
wire_traits<ores::marketdata::messaging::get_published_curve_sets_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_published_curve_sets_request>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::get_published_curve_sets_response>::write_json(
    const ores::marketdata::messaging::get_published_curve_sets_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_published_curve_sets_response
wire_traits<ores::marketdata::messaging::get_published_curve_sets_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_published_curve_sets_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_published_curve_sets_response>::write_msgpack(
    const ores::marketdata::messaging::get_published_curve_sets_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_published_curve_sets_response
wire_traits<ores::marketdata::messaging::get_published_curve_sets_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_published_curve_sets_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::withdraw_curve_request>::write_json(
    const ores::marketdata::messaging::withdraw_curve_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::withdraw_curve_request
wire_traits<ores::marketdata::messaging::withdraw_curve_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::withdraw_curve_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::withdraw_curve_request>::write_msgpack(
    const ores::marketdata::messaging::withdraw_curve_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::withdraw_curve_request
wire_traits<ores::marketdata::messaging::withdraw_curve_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::withdraw_curve_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::withdraw_curve_response>::write_json(
    const ores::marketdata::messaging::withdraw_curve_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::withdraw_curve_response
wire_traits<ores::marketdata::messaging::withdraw_curve_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::withdraw_curve_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::withdraw_curve_response>::write_msgpack(
    const ores::marketdata::messaging::withdraw_curve_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::withdraw_curve_response
wire_traits<ores::marketdata::messaging::withdraw_curve_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::withdraw_curve_response>(data);
}

}
