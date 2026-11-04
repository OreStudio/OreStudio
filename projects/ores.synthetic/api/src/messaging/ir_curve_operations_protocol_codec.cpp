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
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::parameter_spec>::write_json(
    const ores::synthetic::messaging::parameter_spec& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::parameter_spec
wire_traits<ores::synthetic::messaging::parameter_spec>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::parameter_spec>(text);
}

std::string wire_traits<ores::synthetic::messaging::parameter_spec>::write_msgpack(
    const ores::synthetic::messaging::parameter_spec& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::parameter_spec
wire_traits<ores::synthetic::messaging::parameter_spec>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::parameter_spec>(data);
}

std::string wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_request>::write_json(
    const ores::synthetic::messaging::simulate_ir_curve_paths_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::simulate_ir_curve_paths_request
wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::simulate_ir_curve_paths_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_request>::write_msgpack(
    const ores::synthetic::messaging::simulate_ir_curve_paths_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::simulate_ir_curve_paths_request
wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::simulate_ir_curve_paths_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_response>::write_json(
    const ores::synthetic::messaging::simulate_ir_curve_paths_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::simulate_ir_curve_paths_response
wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::simulate_ir_curve_paths_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_response>::write_msgpack(
    const ores::synthetic::messaging::simulate_ir_curve_paths_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::simulate_ir_curve_paths_response
wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::simulate_ir_curve_paths_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_template_row>::write_json(
    const ores::synthetic::messaging::preview_ir_curve_template_row& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::preview_ir_curve_template_row
wire_traits<ores::synthetic::messaging::preview_ir_curve_template_row>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::preview_ir_curve_template_row>(text);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_template_row>::write_msgpack(
    const ores::synthetic::messaging::preview_ir_curve_template_row& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::preview_ir_curve_template_row
wire_traits<ores::synthetic::messaging::preview_ir_curve_template_row>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::preview_ir_curve_template_row>(data);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_request>::write_json(
    const ores::synthetic::messaging::preview_ir_curve_shape_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::preview_ir_curve_shape_request
wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::preview_ir_curve_shape_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_request>::write_msgpack(
    const ores::synthetic::messaging::preview_ir_curve_shape_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::preview_ir_curve_shape_request
wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::preview_ir_curve_shape_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_point>::write_json(
    const ores::synthetic::messaging::preview_ir_curve_shape_point& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::preview_ir_curve_shape_point
wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_point>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::preview_ir_curve_shape_point>(text);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_point>::write_msgpack(
    const ores::synthetic::messaging::preview_ir_curve_shape_point& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::preview_ir_curve_shape_point
wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_point>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::preview_ir_curve_shape_point>(data);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_response>::write_json(
    const ores::synthetic::messaging::preview_ir_curve_shape_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::preview_ir_curve_shape_response
wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::preview_ir_curve_shape_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_response>::write_msgpack(
    const ores::synthetic::messaging::preview_ir_curve_shape_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::preview_ir_curve_shape_response
wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::preview_ir_curve_shape_response>(data);
}

}
