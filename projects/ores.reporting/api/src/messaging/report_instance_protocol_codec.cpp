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
#include "ores.reporting.api/domain/report_instance_codec.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::report_instance_key>::write_json(
    const ores::reporting::messaging::report_instance_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_key
wire_traits<ores::reporting::messaging::report_instance_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_key>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_key>::write_msgpack(
    const ores::reporting::messaging::report_instance_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_key
wire_traits<ores::reporting::messaging::report_instance_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_key>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_write>::write_json(
    const ores::reporting::messaging::report_instance_write& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_write
wire_traits<ores::reporting::messaging::report_instance_write>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_write>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_write>::write_msgpack(
    const ores::reporting::messaging::report_instance_write& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_write
wire_traits<ores::reporting::messaging::report_instance_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_write>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_change>::write_json(
    const ores::reporting::messaging::report_instance_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_change
wire_traits<ores::reporting::messaging::report_instance_change>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_change>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_change>::write_msgpack(
    const ores::reporting::messaging::report_instance_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_change
wire_traits<ores::reporting::messaging::report_instance_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_change>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_removal>::write_json(
    const ores::reporting::messaging::report_instance_removal& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_removal
wire_traits<ores::reporting::messaging::report_instance_removal>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_removal>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_removal>::write_msgpack(
    const ores::reporting::messaging::report_instance_removal& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_removal
wire_traits<ores::reporting::messaging::report_instance_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_removal>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_lookup>::write_json(
    const ores::reporting::messaging::report_instance_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_lookup
wire_traits<ores::reporting::messaging::report_instance_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_lookup>::write_msgpack(
    const ores::reporting::messaging::report_instance_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_lookup
wire_traits<ores::reporting::messaging::report_instance_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instances_filter>::write_json(
    const ores::reporting::messaging::report_instances_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instances_filter
wire_traits<ores::reporting::messaging::report_instances_filter>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instances_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instances_filter>::write_msgpack(
    const ores::reporting::messaging::report_instances_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instances_filter
wire_traits<ores::reporting::messaging::report_instances_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instances_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_event>::write_json(
    const ores::reporting::messaging::report_instance_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_event
wire_traits<ores::reporting::messaging::report_instance_event>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_event>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_event>::write_msgpack(
    const ores::reporting::messaging::report_instance_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_event
wire_traits<ores::reporting::messaging::report_instance_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_event>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_version_key>::write_json(
    const ores::reporting::messaging::report_instance_version_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_version_key
wire_traits<ores::reporting::messaging::report_instance_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_version_key>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_version_key>::write_msgpack(
    const ores::reporting::messaging::report_instance_version_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_version_key
wire_traits<ores::reporting::messaging::report_instance_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_version_key>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_versions_filter>::write_json(
    const ores::reporting::messaging::report_instance_versions_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_versions_filter
wire_traits<ores::reporting::messaging::report_instance_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_versions_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_versions_filter>::write_msgpack(
    const ores::reporting::messaging::report_instance_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_versions_filter
wire_traits<ores::reporting::messaging::report_instance_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_versions_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_instances_request>::write_json(
    const ores::reporting::messaging::list_report_instances_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_instances_request
wire_traits<ores::reporting::messaging::list_report_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_instances_request>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_instances_request>::write_msgpack(
    const ores::reporting::messaging::list_report_instances_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_instances_request
wire_traits<ores::reporting::messaging::list_report_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_instances_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_instances_response>::write_json(
    const ores::reporting::messaging::list_report_instances_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_instances_response
wire_traits<ores::reporting::messaging::list_report_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_instances_response>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_instances_response>::write_msgpack(
    const ores::reporting::messaging::list_report_instances_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_instances_response
wire_traits<ores::reporting::messaging::list_report_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_instances_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_instance_request>::write_json(
    const ores::reporting::messaging::get_report_instance_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_instance_request
wire_traits<ores::reporting::messaging::get_report_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_instance_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_instance_request>::write_msgpack(
    const ores::reporting::messaging::get_report_instance_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_instance_request
wire_traits<ores::reporting::messaging::get_report_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_instance_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_instance_response>::write_json(
    const ores::reporting::messaging::get_report_instance_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_instance_response
wire_traits<ores::reporting::messaging::get_report_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_instance_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_instance_response>::write_msgpack(
    const ores::reporting::messaging::get_report_instance_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_instance_response
wire_traits<ores::reporting::messaging::get_report_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_instance_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_instances_request>::write_json(
    const ores::reporting::messaging::get_many_report_instances_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_instances_request
wire_traits<ores::reporting::messaging::get_many_report_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_instances_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_instances_request>::write_msgpack(
    const ores::reporting::messaging::get_many_report_instances_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_instances_request
wire_traits<ores::reporting::messaging::get_many_report_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_instances_request>(
        data);
}

std::string wire_traits<ores::reporting::messaging::get_many_report_instances_response>::write_json(
    const ores::reporting::messaging::get_many_report_instances_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_instances_response
wire_traits<ores::reporting::messaging::get_many_report_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_instances_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_instances_response>::write_msgpack(
    const ores::reporting::messaging::get_many_report_instances_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_instances_response
wire_traits<ores::reporting::messaging::get_many_report_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_instances_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::put_report_instance_request>::write_json(
    const ores::reporting::messaging::put_report_instance_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_instance_request
wire_traits<ores::reporting::messaging::put_report_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_instance_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_instance_request>::write_msgpack(
    const ores::reporting::messaging::put_report_instance_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_instance_request
wire_traits<ores::reporting::messaging::put_report_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_instance_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_report_instance_response>::write_json(
    const ores::reporting::messaging::put_report_instance_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_instance_response
wire_traits<ores::reporting::messaging::put_report_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_instance_response>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_instance_response>::write_msgpack(
    const ores::reporting::messaging::put_report_instance_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_instance_response
wire_traits<ores::reporting::messaging::put_report_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_instance_response>(data);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_instances_request>::write_json(
    const ores::reporting::messaging::put_many_report_instances_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_instances_request
wire_traits<ores::reporting::messaging::put_many_report_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_instances_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_instances_request>::write_msgpack(
    const ores::reporting::messaging::put_many_report_instances_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_instances_request
wire_traits<ores::reporting::messaging::put_many_report_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_instances_request>(
        data);
}

std::string wire_traits<ores::reporting::messaging::put_many_report_instances_response>::write_json(
    const ores::reporting::messaging::put_many_report_instances_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_instances_response
wire_traits<ores::reporting::messaging::put_many_report_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_instances_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_instances_response>::write_msgpack(
    const ores::reporting::messaging::put_many_report_instances_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_instances_response
wire_traits<ores::reporting::messaging::put_many_report_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_instances_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_instance_request>::write_json(
    const ores::reporting::messaging::delete_report_instance_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_instance_request
wire_traits<ores::reporting::messaging::delete_report_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_instance_request>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_report_instance_request>::write_msgpack(
    const ores::reporting::messaging::delete_report_instance_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_instance_request
wire_traits<ores::reporting::messaging::delete_report_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_instance_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_instance_response>::write_json(
    const ores::reporting::messaging::delete_report_instance_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_instance_response
wire_traits<ores::reporting::messaging::delete_report_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_instance_response>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_report_instance_response>::write_msgpack(
    const ores::reporting::messaging::delete_report_instance_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_instance_response
wire_traits<ores::reporting::messaging::delete_report_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_instance_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_instances_request>::write_json(
    const ores::reporting::messaging::delete_many_report_instances_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_instances_request
wire_traits<ores::reporting::messaging::delete_many_report_instances_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_instances_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_instances_request>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_instances_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_instances_request
wire_traits<ores::reporting::messaging::delete_many_report_instances_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_instances_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_instances_response>::write_json(
    const ores::reporting::messaging::delete_many_report_instances_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_instances_response
wire_traits<ores::reporting::messaging::delete_many_report_instances_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_instances_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_instances_response>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_instances_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_instances_response
wire_traits<ores::reporting::messaging::delete_many_report_instances_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_instances_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::list_report_instance_versions_request>::write_json(
    const ores::reporting::messaging::list_report_instance_versions_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_instance_versions_request
wire_traits<ores::reporting::messaging::list_report_instance_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_instance_versions_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::list_report_instance_versions_request>::write_msgpack(
    const ores::reporting::messaging::list_report_instance_versions_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_instance_versions_request
wire_traits<ores::reporting::messaging::list_report_instance_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_instance_versions_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::list_report_instance_versions_response>::write_json(
    const ores::reporting::messaging::list_report_instance_versions_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_instance_versions_response
wire_traits<ores::reporting::messaging::list_report_instance_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_instance_versions_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::list_report_instance_versions_response>::write_msgpack(
    const ores::reporting::messaging::list_report_instance_versions_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_instance_versions_response
wire_traits<ores::reporting::messaging::list_report_instance_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_instance_versions_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_version_request>::write_json(
    const ores::reporting::messaging::get_report_instance_version_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_instance_version_request
wire_traits<ores::reporting::messaging::get_report_instance_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_instance_version_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_version_request>::write_msgpack(
    const ores::reporting::messaging::get_report_instance_version_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_instance_version_request
wire_traits<ores::reporting::messaging::get_report_instance_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_instance_version_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_version_response>::write_json(
    const ores::reporting::messaging::get_report_instance_version_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_instance_version_response
wire_traits<ores::reporting::messaging::get_report_instance_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_instance_version_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_version_response>::write_msgpack(
    const ores::reporting::messaging::get_report_instance_version_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_instance_version_response
wire_traits<ores::reporting::messaging::get_report_instance_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_instance_version_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_request>::
    write_json(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_latest_report_instances_by_definition_request
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_latest_report_instances_by_definition_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_request>::
    write_msgpack(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_latest_report_instances_by_definition_request
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_latest_report_instances_by_definition_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_response>::
    write_json(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_latest_report_instances_by_definition_response
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_latest_report_instances_by_definition_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_response>::
    write_msgpack(
        const ores::reporting::messaging::get_latest_report_instances_by_definition_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_latest_report_instances_by_definition_response
wire_traits<ores::reporting::messaging::get_latest_report_instances_by_definition_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_latest_report_instances_by_definition_response>(data);
}

std::string wire_traits<ores::reporting::messaging::report_instance_health>::write_json(
    const ores::reporting::messaging::report_instance_health& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_instance_health
wire_traits<ores::reporting::messaging::report_instance_health>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_instance_health>(text);
}

std::string wire_traits<ores::reporting::messaging::report_instance_health>::write_msgpack(
    const ores::reporting::messaging::report_instance_health& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_instance_health
wire_traits<ores::reporting::messaging::report_instance_health>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_instance_health>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_instance_health_request>::write_json(
    const ores::reporting::messaging::get_report_instance_health_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_instance_health_request
wire_traits<ores::reporting::messaging::get_report_instance_health_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_instance_health_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_health_request>::write_msgpack(
    const ores::reporting::messaging::get_report_instance_health_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_instance_health_request
wire_traits<ores::reporting::messaging::get_report_instance_health_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_instance_health_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_health_response>::write_json(
    const ores::reporting::messaging::get_report_instance_health_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_instance_health_response
wire_traits<ores::reporting::messaging::get_report_instance_health_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_instance_health_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_report_instance_health_response>::write_msgpack(
    const ores::reporting::messaging::get_report_instance_health_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_instance_health_response
wire_traits<ores::reporting::messaging::get_report_instance_health_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_instance_health_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::report_health_count>::write_json(
    const ores::reporting::messaging::report_health_count& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_health_count
wire_traits<ores::reporting::messaging::report_health_count>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_health_count>(text);
}

std::string wire_traits<ores::reporting::messaging::report_health_count>::write_msgpack(
    const ores::reporting::messaging::report_health_count& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_health_count
wire_traits<ores::reporting::messaging::report_health_count>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_health_count>(data);
}

std::string
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_request>::write_json(
    const ores::reporting::messaging::count_report_definitions_by_health_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::count_report_definitions_by_health_request
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::count_report_definitions_by_health_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_request>::write_msgpack(
    const ores::reporting::messaging::count_report_definitions_by_health_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::count_report_definitions_by_health_request
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::count_report_definitions_by_health_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_response>::write_json(
    const ores::reporting::messaging::count_report_definitions_by_health_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::count_report_definitions_by_health_response
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::count_report_definitions_by_health_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_response>::write_msgpack(
    const ores::reporting::messaging::count_report_definitions_by_health_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::count_report_definitions_by_health_response
wire_traits<ores::reporting::messaging::count_report_definitions_by_health_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::count_report_definitions_by_health_response>(data);
}

}
