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
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::compute::messaging::get_grid_stats_request>::write_json(
    const ores::compute::messaging::get_grid_stats_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_grid_stats_request
wire_traits<ores::compute::messaging::get_grid_stats_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_grid_stats_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_grid_stats_request>::write_msgpack(
    const ores::compute::messaging::get_grid_stats_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_grid_stats_request
wire_traits<ores::compute::messaging::get_grid_stats_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_grid_stats_request>(data);
}

std::string wire_traits<ores::compute::messaging::node_stats_summary>::write_json(
    const ores::compute::messaging::node_stats_summary& v) {
    return object::write_json(v);
}

ores::compute::messaging::node_stats_summary
wire_traits<ores::compute::messaging::node_stats_summary>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::node_stats_summary>(text);
}

std::string wire_traits<ores::compute::messaging::node_stats_summary>::write_msgpack(
    const ores::compute::messaging::node_stats_summary& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::node_stats_summary
wire_traits<ores::compute::messaging::node_stats_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::node_stats_summary>(data);
}

std::string wire_traits<ores::compute::messaging::get_grid_stats_response>::write_json(
    const ores::compute::messaging::get_grid_stats_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_grid_stats_response
wire_traits<ores::compute::messaging::get_grid_stats_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_grid_stats_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_grid_stats_response>::write_msgpack(
    const ores::compute::messaging::get_grid_stats_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_grid_stats_response
wire_traits<ores::compute::messaging::get_grid_stats_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_grid_stats_response>(data);
}

std::string wire_traits<ores::compute::messaging::get_grid_series_request>::write_json(
    const ores::compute::messaging::get_grid_series_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_grid_series_request
wire_traits<ores::compute::messaging::get_grid_series_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_grid_series_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_grid_series_request>::write_msgpack(
    const ores::compute::messaging::get_grid_series_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_grid_series_request
wire_traits<ores::compute::messaging::get_grid_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_grid_series_request>(data);
}

std::string wire_traits<ores::compute::messaging::get_grid_series_response>::write_json(
    const ores::compute::messaging::get_grid_series_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_grid_series_response
wire_traits<ores::compute::messaging::get_grid_series_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_grid_series_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_grid_series_response>::write_msgpack(
    const ores::compute::messaging::get_grid_series_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_grid_series_response
wire_traits<ores::compute::messaging::get_grid_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_grid_series_response>(data);
}

std::string wire_traits<ores::compute::messaging::get_node_series_request>::write_json(
    const ores::compute::messaging::get_node_series_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_node_series_request
wire_traits<ores::compute::messaging::get_node_series_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_node_series_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_node_series_request>::write_msgpack(
    const ores::compute::messaging::get_node_series_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_node_series_request
wire_traits<ores::compute::messaging::get_node_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_node_series_request>(data);
}

std::string wire_traits<ores::compute::messaging::node_series_node>::write_json(
    const ores::compute::messaging::node_series_node& v) {
    return object::write_json(v);
}

ores::compute::messaging::node_series_node
wire_traits<ores::compute::messaging::node_series_node>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::node_series_node>(text);
}

std::string wire_traits<ores::compute::messaging::node_series_node>::write_msgpack(
    const ores::compute::messaging::node_series_node& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::node_series_node
wire_traits<ores::compute::messaging::node_series_node>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::node_series_node>(data);
}

std::string wire_traits<ores::compute::messaging::get_node_series_response>::write_json(
    const ores::compute::messaging::get_node_series_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_node_series_response
wire_traits<ores::compute::messaging::get_node_series_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_node_series_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_node_series_response>::write_msgpack(
    const ores::compute::messaging::get_node_series_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_node_series_response
wire_traits<ores::compute::messaging::get_node_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_node_series_response>(data);
}

std::string wire_traits<ores::compute::messaging::node_sample_message>::write_json(
    const ores::compute::messaging::node_sample_message& v) {
    return object::write_json(v);
}

ores::compute::messaging::node_sample_message
wire_traits<ores::compute::messaging::node_sample_message>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::node_sample_message>(text);
}

std::string wire_traits<ores::compute::messaging::node_sample_message>::write_msgpack(
    const ores::compute::messaging::node_sample_message& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::node_sample_message
wire_traits<ores::compute::messaging::node_sample_message>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::node_sample_message>(data);
}

std::string wire_traits<ores::compute::messaging::get_grid_usage_request>::write_json(
    const ores::compute::messaging::get_grid_usage_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_grid_usage_request
wire_traits<ores::compute::messaging::get_grid_usage_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_grid_usage_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_grid_usage_request>::write_msgpack(
    const ores::compute::messaging::get_grid_usage_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_grid_usage_request
wire_traits<ores::compute::messaging::get_grid_usage_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_grid_usage_request>(data);
}

std::string wire_traits<ores::compute::messaging::get_grid_usage_response>::write_json(
    const ores::compute::messaging::get_grid_usage_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_grid_usage_response
wire_traits<ores::compute::messaging::get_grid_usage_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_grid_usage_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_grid_usage_response>::write_msgpack(
    const ores::compute::messaging::get_grid_usage_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_grid_usage_response
wire_traits<ores::compute::messaging::get_grid_usage_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_grid_usage_response>(data);
}

}
