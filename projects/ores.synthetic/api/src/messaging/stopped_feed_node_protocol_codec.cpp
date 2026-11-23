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
#include "ores.synthetic.api/domain/stopped_feed_node_codec.hpp"
#include "ores.synthetic.api/messaging/stopped_feed_node_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_key>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_key
wire_traits<ores::synthetic::messaging::stopped_feed_node_key>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_key>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_key
wire_traits<ores::synthetic::messaging::stopped_feed_node_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_write>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_write& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_write
wire_traits<ores::synthetic::messaging::stopped_feed_node_write>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_write>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_write>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_write& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_write
wire_traits<ores::synthetic::messaging::stopped_feed_node_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_write>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_change>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_change& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_change
wire_traits<ores::synthetic::messaging::stopped_feed_node_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_change>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_change>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_change& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_change
wire_traits<ores::synthetic::messaging::stopped_feed_node_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_change>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_removal>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_removal& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_removal
wire_traits<ores::synthetic::messaging::stopped_feed_node_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_removal>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_removal>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_removal& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_removal
wire_traits<ores::synthetic::messaging::stopped_feed_node_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_removal>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_lookup>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_lookup& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_lookup
wire_traits<ores::synthetic::messaging::stopped_feed_node_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_lookup>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_lookup>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_lookup& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_lookup
wire_traits<ores::synthetic::messaging::stopped_feed_node_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_lookup>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_nodes_filter>::write_json(
    const ores::synthetic::messaging::stopped_feed_nodes_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_nodes_filter
wire_traits<ores::synthetic::messaging::stopped_feed_nodes_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_nodes_filter>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_nodes_filter>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_nodes_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_nodes_filter
wire_traits<ores::synthetic::messaging::stopped_feed_nodes_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_nodes_filter>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_event>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_event& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_event
wire_traits<ores::synthetic::messaging::stopped_feed_node_event>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_event>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_event>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_event& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_event
wire_traits<ores::synthetic::messaging::stopped_feed_node_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_event>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_version_key>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_version_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_version_key
wire_traits<ores::synthetic::messaging::stopped_feed_node_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_version_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_version_key>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_version_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_version_key
wire_traits<ores::synthetic::messaging::stopped_feed_node_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_version_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::stopped_feed_node_versions_filter>::write_json(
    const ores::synthetic::messaging::stopped_feed_node_versions_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::stopped_feed_node_versions_filter
wire_traits<ores::synthetic::messaging::stopped_feed_node_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::stopped_feed_node_versions_filter>(text);
}

std::string
wire_traits<ores::synthetic::messaging::stopped_feed_node_versions_filter>::write_msgpack(
    const ores::synthetic::messaging::stopped_feed_node_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::stopped_feed_node_versions_filter
wire_traits<ores::synthetic::messaging::stopped_feed_node_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::stopped_feed_node_versions_filter>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_request>::write_json(
    const ores::synthetic::messaging::list_stopped_feed_nodes_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_stopped_feed_nodes_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_request>::write_msgpack(
    const ores::synthetic::messaging::list_stopped_feed_nodes_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_stopped_feed_nodes_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_response>::write_json(
    const ores::synthetic::messaging::list_stopped_feed_nodes_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_stopped_feed_nodes_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_response>::write_msgpack(
    const ores::synthetic::messaging::list_stopped_feed_nodes_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_stopped_feed_nodes_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_stopped_feed_node_request>::write_json(
    const ores::synthetic::messaging::get_stopped_feed_node_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_stopped_feed_node_request
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_stopped_feed_node_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_stopped_feed_node_request>::write_msgpack(
    const ores::synthetic::messaging::get_stopped_feed_node_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_stopped_feed_node_request
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_stopped_feed_node_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_stopped_feed_node_response>::write_json(
    const ores::synthetic::messaging::get_stopped_feed_node_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_stopped_feed_node_response
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_stopped_feed_node_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_stopped_feed_node_response>::write_msgpack(
    const ores::synthetic::messaging::get_stopped_feed_node_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_stopped_feed_node_response
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_stopped_feed_node_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_request>::write_json(
    const ores::synthetic::messaging::get_many_stopped_feed_nodes_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_stopped_feed_nodes_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_request>::write_msgpack(
    const ores::synthetic::messaging::get_many_stopped_feed_nodes_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_many_stopped_feed_nodes_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_response>::write_json(
    const ores::synthetic::messaging::get_many_stopped_feed_nodes_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_stopped_feed_nodes_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_response>::write_msgpack(
    const ores::synthetic::messaging::get_many_stopped_feed_nodes_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_many_stopped_feed_nodes_response>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::put_stopped_feed_node_request>::write_json(
    const ores::synthetic::messaging::put_stopped_feed_node_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_stopped_feed_node_request
wire_traits<ores::synthetic::messaging::put_stopped_feed_node_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_stopped_feed_node_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_stopped_feed_node_request>::write_msgpack(
    const ores::synthetic::messaging::put_stopped_feed_node_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_stopped_feed_node_request
wire_traits<ores::synthetic::messaging::put_stopped_feed_node_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_stopped_feed_node_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_stopped_feed_node_response>::write_json(
    const ores::synthetic::messaging::put_stopped_feed_node_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_stopped_feed_node_response
wire_traits<ores::synthetic::messaging::put_stopped_feed_node_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_stopped_feed_node_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_stopped_feed_node_response>::write_msgpack(
    const ores::synthetic::messaging::put_stopped_feed_node_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_stopped_feed_node_response
wire_traits<ores::synthetic::messaging::put_stopped_feed_node_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_stopped_feed_node_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_request>::write_json(
    const ores::synthetic::messaging::put_many_stopped_feed_nodes_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_stopped_feed_nodes_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_request>::write_msgpack(
    const ores::synthetic::messaging::put_many_stopped_feed_nodes_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_many_stopped_feed_nodes_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_response>::write_json(
    const ores::synthetic::messaging::put_many_stopped_feed_nodes_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_stopped_feed_nodes_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_response>::write_msgpack(
    const ores::synthetic::messaging::put_many_stopped_feed_nodes_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_many_stopped_feed_nodes_response>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_request>::write_json(
    const ores::synthetic::messaging::delete_stopped_feed_node_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_stopped_feed_node_request
wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_stopped_feed_node_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_request>::write_msgpack(
    const ores::synthetic::messaging::delete_stopped_feed_node_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_stopped_feed_node_request
wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_stopped_feed_node_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_response>::write_json(
    const ores::synthetic::messaging::delete_stopped_feed_node_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_stopped_feed_node_response
wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_stopped_feed_node_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_response>::write_msgpack(
    const ores::synthetic::messaging::delete_stopped_feed_node_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_stopped_feed_node_response
wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_stopped_feed_node_response>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request>::write_json(
    const ores::synthetic::messaging::delete_many_stopped_feed_nodes_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request>::write_msgpack(
    const ores::synthetic::messaging::delete_many_stopped_feed_nodes_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_stopped_feed_nodes_request
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_response>::write_json(
    const ores::synthetic::messaging::delete_many_stopped_feed_nodes_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_many_stopped_feed_nodes_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_response>::write_msgpack(
    const ores::synthetic::messaging::delete_many_stopped_feed_nodes_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_stopped_feed_nodes_response
wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_stopped_feed_nodes_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_request>::write_json(
    const ores::synthetic::messaging::list_stopped_feed_node_versions_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_stopped_feed_node_versions_request
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_stopped_feed_node_versions_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_request>::write_msgpack(
    const ores::synthetic::messaging::list_stopped_feed_node_versions_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_stopped_feed_node_versions_request
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_stopped_feed_node_versions_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_response>::write_json(
    const ores::synthetic::messaging::list_stopped_feed_node_versions_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_stopped_feed_node_versions_response
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_stopped_feed_node_versions_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_response>::write_msgpack(
    const ores::synthetic::messaging::list_stopped_feed_node_versions_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_stopped_feed_node_versions_response
wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_stopped_feed_node_versions_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_request>::write_json(
    const ores::synthetic::messaging::get_stopped_feed_node_version_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_stopped_feed_node_version_request
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_stopped_feed_node_version_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_request>::write_msgpack(
    const ores::synthetic::messaging::get_stopped_feed_node_version_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_stopped_feed_node_version_request
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_stopped_feed_node_version_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_response>::write_json(
    const ores::synthetic::messaging::get_stopped_feed_node_version_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_stopped_feed_node_version_response
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_stopped_feed_node_version_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_response>::write_msgpack(
    const ores::synthetic::messaging::get_stopped_feed_node_version_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_stopped_feed_node_version_response
wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_stopped_feed_node_version_response>(
        data);
}

}
