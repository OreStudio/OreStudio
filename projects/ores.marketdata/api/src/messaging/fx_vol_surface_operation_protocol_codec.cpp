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
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::fx_vol_pillar_fit>::write_json(
    const ores::marketdata::messaging::fx_vol_pillar_fit& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_pillar_fit
wire_traits<ores::marketdata::messaging::fx_vol_pillar_fit>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_pillar_fit>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_pillar_fit>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_pillar_fit& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_pillar_fit
wire_traits<ores::marketdata::messaging::fx_vol_pillar_fit>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_pillar_fit>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_grid_point>::write_json(
    const ores::marketdata::messaging::fx_vol_grid_point& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_grid_point
wire_traits<ores::marketdata::messaging::fx_vol_grid_point>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_grid_point>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_grid_point>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_grid_point& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_grid_point
wire_traits<ores::marketdata::messaging::fx_vol_grid_point>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_grid_point>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_violation>::write_json(
    const ores::marketdata::messaging::fx_vol_violation& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_violation
wire_traits<ores::marketdata::messaging::fx_vol_violation>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_violation>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_violation>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_violation& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_violation
wire_traits<ores::marketdata::messaging::fx_vol_violation>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_violation>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_missing_pillar>::write_json(
    const ores::marketdata::messaging::fx_vol_missing_pillar& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_missing_pillar
wire_traits<ores::marketdata::messaging::fx_vol_missing_pillar>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_missing_pillar>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_missing_pillar>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_missing_pillar& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_missing_pillar
wire_traits<ores::marketdata::messaging::fx_vol_missing_pillar>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_missing_pillar>(data);
}

std::string wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_request>::write_json(
    const ores::marketdata::messaging::calibrate_fx_vol_surface_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::calibrate_fx_vol_surface_request
wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::calibrate_fx_vol_surface_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_request>::write_msgpack(
    const ores::marketdata::messaging::calibrate_fx_vol_surface_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::calibrate_fx_vol_surface_request
wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::calibrate_fx_vol_surface_request>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_response>::write_json(
    const ores::marketdata::messaging::calibrate_fx_vol_surface_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::calibrate_fx_vol_surface_response
wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::calibrate_fx_vol_surface_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_response>::write_msgpack(
    const ores::marketdata::messaging::calibrate_fx_vol_surface_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::calibrate_fx_vol_surface_response
wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::calibrate_fx_vol_surface_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_request>::write_json(
    const ores::marketdata::messaging::publish_fx_vol_surface_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::publish_fx_vol_surface_request
wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::publish_fx_vol_surface_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_request>::write_msgpack(
    const ores::marketdata::messaging::publish_fx_vol_surface_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::publish_fx_vol_surface_request
wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::publish_fx_vol_surface_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_response>::write_json(
    const ores::marketdata::messaging::publish_fx_vol_surface_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::publish_fx_vol_surface_response
wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::publish_fx_vol_surface_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_response>::write_msgpack(
    const ores::marketdata::messaging::publish_fx_vol_surface_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::publish_fx_vol_surface_response
wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::publish_fx_vol_surface_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request>::write_json(
    const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request>::write_msgpack(
    const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request>(data);
}

std::string
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>::write_json(
    const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>::
    write_msgpack(
        const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response
wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_surface_point>::write_json(
    const ores::marketdata::messaging::fx_vol_surface_point& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::fx_vol_surface_point
wire_traits<ores::marketdata::messaging::fx_vol_surface_point>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::fx_vol_surface_point>(text);
}

std::string wire_traits<ores::marketdata::messaging::fx_vol_surface_point>::write_msgpack(
    const ores::marketdata::messaging::fx_vol_surface_point& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::fx_vol_surface_point
wire_traits<ores::marketdata::messaging::fx_vol_surface_point>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::fx_vol_surface_point>(data);
}

std::string
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_request>::write_json(
    const ores::marketdata::messaging::get_published_fx_vol_surface_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_published_fx_vol_surface_request
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_published_fx_vol_surface_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_request>::write_msgpack(
    const ores::marketdata::messaging::get_published_fx_vol_surface_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_published_fx_vol_surface_request
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_published_fx_vol_surface_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_response>::write_json(
    const ores::marketdata::messaging::get_published_fx_vol_surface_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_published_fx_vol_surface_response
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_published_fx_vol_surface_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_response>::write_msgpack(
    const ores::marketdata::messaging::get_published_fx_vol_surface_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_published_fx_vol_surface_response
wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_published_fx_vol_surface_response>(
        data);
}

}
