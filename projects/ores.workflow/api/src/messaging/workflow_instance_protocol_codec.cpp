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
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.workflow.api/domain/workflow_instance_codec.hpp"
#include "ores.workflow.api/messaging/workflow_instance_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::workflow::messaging::workflow_instance_key>::write_json(
    const ores::workflow::messaging::workflow_instance_key& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_key
wire_traits<ores::workflow::messaging::workflow_instance_key>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_key>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_key>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_key& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_key
wire_traits<ores::workflow::messaging::workflow_instance_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_key>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_write>::write_json(
    const ores::workflow::messaging::workflow_instance_write& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_write
wire_traits<ores::workflow::messaging::workflow_instance_write>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_write>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_write>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_write& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_write
wire_traits<ores::workflow::messaging::workflow_instance_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_write>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_change>::write_json(
    const ores::workflow::messaging::workflow_instance_change& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_change
wire_traits<ores::workflow::messaging::workflow_instance_change>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_change>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_change>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_change& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_change
wire_traits<ores::workflow::messaging::workflow_instance_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_change>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_removal>::write_json(
    const ores::workflow::messaging::workflow_instance_removal& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_removal
wire_traits<ores::workflow::messaging::workflow_instance_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_removal>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_removal>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_removal& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_removal
wire_traits<ores::workflow::messaging::workflow_instance_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_removal>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_lookup>::write_json(
    const ores::workflow::messaging::workflow_instance_lookup& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_lookup
wire_traits<ores::workflow::messaging::workflow_instance_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_lookup>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_lookup>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_lookup& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_lookup
wire_traits<ores::workflow::messaging::workflow_instance_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_lookup>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instances_filter>::write_json(
    const ores::workflow::messaging::workflow_instances_filter& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instances_filter
wire_traits<ores::workflow::messaging::workflow_instances_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instances_filter>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instances_filter>::write_msgpack(
    const ores::workflow::messaging::workflow_instances_filter& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instances_filter
wire_traits<ores::workflow::messaging::workflow_instances_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instances_filter>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_event>::write_json(
    const ores::workflow::messaging::workflow_instance_event& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_event
wire_traits<ores::workflow::messaging::workflow_instance_event>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_event>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_event>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_event& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_event
wire_traits<ores::workflow::messaging::workflow_instance_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_event>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_version_key>::write_json(
    const ores::workflow::messaging::workflow_instance_version_key& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_version_key
wire_traits<ores::workflow::messaging::workflow_instance_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_version_key>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_version_key>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_version_key& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_version_key
wire_traits<ores::workflow::messaging::workflow_instance_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_version_key>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_versions_filter>::write_json(
    const ores::workflow::messaging::workflow_instance_versions_filter& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_versions_filter
wire_traits<ores::workflow::messaging::workflow_instance_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_versions_filter>(text);
}

std::string
wire_traits<ores::workflow::messaging::workflow_instance_versions_filter>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_versions_filter
wire_traits<ores::workflow::messaging::workflow_instance_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_versions_filter>(data);
}

std::string wire_traits<ores::workflow::messaging::list_workflow_instances_request>::write_json(
    const ores::workflow::messaging::list_workflow_instances_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_instances_request
wire_traits<ores::workflow::messaging::list_workflow_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_instances_request>(text);
}

std::string wire_traits<ores::workflow::messaging::list_workflow_instances_request>::write_msgpack(
    const ores::workflow::messaging::list_workflow_instances_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_instances_request
wire_traits<ores::workflow::messaging::list_workflow_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::list_workflow_instances_request>(data);
}

std::string wire_traits<ores::workflow::messaging::list_workflow_instances_response>::write_json(
    const ores::workflow::messaging::list_workflow_instances_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_instances_response
wire_traits<ores::workflow::messaging::list_workflow_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_instances_response>(text);
}

std::string wire_traits<ores::workflow::messaging::list_workflow_instances_response>::write_msgpack(
    const ores::workflow::messaging::list_workflow_instances_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_instances_response
wire_traits<ores::workflow::messaging::list_workflow_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::list_workflow_instances_response>(data);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_instance_request>::write_json(
    const ores::workflow::messaging::get_workflow_instance_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_workflow_instance_request
wire_traits<ores::workflow::messaging::get_workflow_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_workflow_instance_request>(text);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_instance_request>::write_msgpack(
    const ores::workflow::messaging::get_workflow_instance_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_workflow_instance_request
wire_traits<ores::workflow::messaging::get_workflow_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_workflow_instance_request>(data);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_instance_response>::write_json(
    const ores::workflow::messaging::get_workflow_instance_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_workflow_instance_response
wire_traits<ores::workflow::messaging::get_workflow_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_workflow_instance_response>(text);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_instance_response>::write_msgpack(
    const ores::workflow::messaging::get_workflow_instance_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_workflow_instance_response
wire_traits<ores::workflow::messaging::get_workflow_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_workflow_instance_response>(data);
}

std::string wire_traits<ores::workflow::messaging::get_many_workflow_instances_request>::write_json(
    const ores::workflow::messaging::get_many_workflow_instances_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_many_workflow_instances_request
wire_traits<ores::workflow::messaging::get_many_workflow_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_many_workflow_instances_request>(text);
}

std::string
wire_traits<ores::workflow::messaging::get_many_workflow_instances_request>::write_msgpack(
    const ores::workflow::messaging::get_many_workflow_instances_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_many_workflow_instances_request
wire_traits<ores::workflow::messaging::get_many_workflow_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_many_workflow_instances_request>(
        data);
}

std::string
wire_traits<ores::workflow::messaging::get_many_workflow_instances_response>::write_json(
    const ores::workflow::messaging::get_many_workflow_instances_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_many_workflow_instances_response
wire_traits<ores::workflow::messaging::get_many_workflow_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_many_workflow_instances_response>(text);
}

std::string
wire_traits<ores::workflow::messaging::get_many_workflow_instances_response>::write_msgpack(
    const ores::workflow::messaging::get_many_workflow_instances_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_many_workflow_instances_response
wire_traits<ores::workflow::messaging::get_many_workflow_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_many_workflow_instances_response>(
        data);
}

std::string wire_traits<ores::workflow::messaging::put_workflow_instance_request>::write_json(
    const ores::workflow::messaging::put_workflow_instance_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::put_workflow_instance_request
wire_traits<ores::workflow::messaging::put_workflow_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::put_workflow_instance_request>(text);
}

std::string wire_traits<ores::workflow::messaging::put_workflow_instance_request>::write_msgpack(
    const ores::workflow::messaging::put_workflow_instance_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::put_workflow_instance_request
wire_traits<ores::workflow::messaging::put_workflow_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::put_workflow_instance_request>(data);
}

std::string wire_traits<ores::workflow::messaging::put_workflow_instance_response>::write_json(
    const ores::workflow::messaging::put_workflow_instance_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::put_workflow_instance_response
wire_traits<ores::workflow::messaging::put_workflow_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::put_workflow_instance_response>(text);
}

std::string wire_traits<ores::workflow::messaging::put_workflow_instance_response>::write_msgpack(
    const ores::workflow::messaging::put_workflow_instance_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::put_workflow_instance_response
wire_traits<ores::workflow::messaging::put_workflow_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::put_workflow_instance_response>(data);
}

std::string wire_traits<ores::workflow::messaging::put_many_workflow_instances_request>::write_json(
    const ores::workflow::messaging::put_many_workflow_instances_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::put_many_workflow_instances_request
wire_traits<ores::workflow::messaging::put_many_workflow_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::put_many_workflow_instances_request>(text);
}

std::string
wire_traits<ores::workflow::messaging::put_many_workflow_instances_request>::write_msgpack(
    const ores::workflow::messaging::put_many_workflow_instances_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::put_many_workflow_instances_request
wire_traits<ores::workflow::messaging::put_many_workflow_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::put_many_workflow_instances_request>(
        data);
}

std::string
wire_traits<ores::workflow::messaging::put_many_workflow_instances_response>::write_json(
    const ores::workflow::messaging::put_many_workflow_instances_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::put_many_workflow_instances_response
wire_traits<ores::workflow::messaging::put_many_workflow_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::put_many_workflow_instances_response>(text);
}

std::string
wire_traits<ores::workflow::messaging::put_many_workflow_instances_response>::write_msgpack(
    const ores::workflow::messaging::put_many_workflow_instances_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::put_many_workflow_instances_response
wire_traits<ores::workflow::messaging::put_many_workflow_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::put_many_workflow_instances_response>(
        data);
}

std::string wire_traits<ores::workflow::messaging::delete_workflow_instance_request>::write_json(
    const ores::workflow::messaging::delete_workflow_instance_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::delete_workflow_instance_request
wire_traits<ores::workflow::messaging::delete_workflow_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::delete_workflow_instance_request>(text);
}

std::string wire_traits<ores::workflow::messaging::delete_workflow_instance_request>::write_msgpack(
    const ores::workflow::messaging::delete_workflow_instance_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::delete_workflow_instance_request
wire_traits<ores::workflow::messaging::delete_workflow_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::delete_workflow_instance_request>(data);
}

std::string wire_traits<ores::workflow::messaging::delete_workflow_instance_response>::write_json(
    const ores::workflow::messaging::delete_workflow_instance_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::delete_workflow_instance_response
wire_traits<ores::workflow::messaging::delete_workflow_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::delete_workflow_instance_response>(text);
}

std::string
wire_traits<ores::workflow::messaging::delete_workflow_instance_response>::write_msgpack(
    const ores::workflow::messaging::delete_workflow_instance_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::delete_workflow_instance_response
wire_traits<ores::workflow::messaging::delete_workflow_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::delete_workflow_instance_response>(data);
}

std::string
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_request>::write_json(
    const ores::workflow::messaging::delete_many_workflow_instances_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::delete_many_workflow_instances_request
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::delete_many_workflow_instances_request>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_request>::write_msgpack(
    const ores::workflow::messaging::delete_many_workflow_instances_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::delete_many_workflow_instances_request
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::delete_many_workflow_instances_request>(
        data);
}

std::string
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_response>::write_json(
    const ores::workflow::messaging::delete_many_workflow_instances_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::delete_many_workflow_instances_response
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::delete_many_workflow_instances_response>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_response>::write_msgpack(
    const ores::workflow::messaging::delete_many_workflow_instances_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::delete_many_workflow_instances_response
wire_traits<ores::workflow::messaging::delete_many_workflow_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::delete_many_workflow_instances_response>(
        data);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_request>::write_json(
    const ores::workflow::messaging::list_workflow_instance_versions_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_instance_versions_request
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_instance_versions_request>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_request>::write_msgpack(
    const ores::workflow::messaging::list_workflow_instance_versions_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_instance_versions_request
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::list_workflow_instance_versions_request>(
        data);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_response>::write_json(
    const ores::workflow::messaging::list_workflow_instance_versions_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_instance_versions_response
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_instance_versions_response>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_response>::write_msgpack(
    const ores::workflow::messaging::list_workflow_instance_versions_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_instance_versions_response
wire_traits<ores::workflow::messaging::list_workflow_instance_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::workflow::messaging::list_workflow_instance_versions_response>(data);
}

std::string
wire_traits<ores::workflow::messaging::get_workflow_instance_version_request>::write_json(
    const ores::workflow::messaging::get_workflow_instance_version_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_workflow_instance_version_request
wire_traits<ores::workflow::messaging::get_workflow_instance_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_workflow_instance_version_request>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::get_workflow_instance_version_request>::write_msgpack(
    const ores::workflow::messaging::get_workflow_instance_version_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_workflow_instance_version_request
wire_traits<ores::workflow::messaging::get_workflow_instance_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_workflow_instance_version_request>(
        data);
}

std::string
wire_traits<ores::workflow::messaging::get_workflow_instance_version_response>::write_json(
    const ores::workflow::messaging::get_workflow_instance_version_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_workflow_instance_version_response
wire_traits<ores::workflow::messaging::get_workflow_instance_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_workflow_instance_version_response>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::get_workflow_instance_version_response>::write_msgpack(
    const ores::workflow::messaging::get_workflow_instance_version_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_workflow_instance_version_response
wire_traits<ores::workflow::messaging::get_workflow_instance_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_workflow_instance_version_response>(
        data);
}

}
