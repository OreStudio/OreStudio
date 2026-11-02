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
#include "ores.reporting.api/domain/cut_approval_detail_codec.hpp"
#include "ores.reporting.api/messaging/cut_approval_detail_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_key>::write_json(
    const ores::reporting::messaging::cut_approval_detail_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_approval_detail_key
wire_traits<ores::reporting::messaging::cut_approval_detail_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_approval_detail_key>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_key>::write_msgpack(
    const ores::reporting::messaging::cut_approval_detail_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_approval_detail_key
wire_traits<ores::reporting::messaging::cut_approval_detail_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_approval_detail_key>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_lookup>::write_json(
    const ores::reporting::messaging::cut_approval_detail_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_approval_detail_lookup
wire_traits<ores::reporting::messaging::cut_approval_detail_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_approval_detail_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_lookup>::write_msgpack(
    const ores::reporting::messaging::cut_approval_detail_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_approval_detail_lookup
wire_traits<ores::reporting::messaging::cut_approval_detail_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_approval_detail_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_details_filter>::write_json(
    const ores::reporting::messaging::cut_approval_details_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_approval_details_filter
wire_traits<ores::reporting::messaging::cut_approval_details_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_approval_details_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_details_filter>::write_msgpack(
    const ores::reporting::messaging::cut_approval_details_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_approval_details_filter
wire_traits<ores::reporting::messaging::cut_approval_details_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_approval_details_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_event>::write_json(
    const ores::reporting::messaging::cut_approval_detail_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_approval_detail_event
wire_traits<ores::reporting::messaging::cut_approval_detail_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_approval_detail_event>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_event>::write_msgpack(
    const ores::reporting::messaging::cut_approval_detail_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_approval_detail_event
wire_traits<ores::reporting::messaging::cut_approval_detail_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_approval_detail_event>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_version_key>::write_json(
    const ores::reporting::messaging::cut_approval_detail_version_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_approval_detail_version_key
wire_traits<ores::reporting::messaging::cut_approval_detail_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_approval_detail_version_key>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_approval_detail_version_key>::write_msgpack(
    const ores::reporting::messaging::cut_approval_detail_version_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_approval_detail_version_key
wire_traits<ores::reporting::messaging::cut_approval_detail_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_approval_detail_version_key>(data);
}

std::string
wire_traits<ores::reporting::messaging::cut_approval_detail_versions_filter>::write_json(
    const ores::reporting::messaging::cut_approval_detail_versions_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_approval_detail_versions_filter
wire_traits<ores::reporting::messaging::cut_approval_detail_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_approval_detail_versions_filter>(text);
}

std::string
wire_traits<ores::reporting::messaging::cut_approval_detail_versions_filter>::write_msgpack(
    const ores::reporting::messaging::cut_approval_detail_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_approval_detail_versions_filter
wire_traits<ores::reporting::messaging::cut_approval_detail_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_approval_detail_versions_filter>(
        data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_approval_details_request>::write_json(
    const ores::reporting::messaging::list_cut_approval_details_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_approval_details_request
wire_traits<ores::reporting::messaging::list_cut_approval_details_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_approval_details_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_approval_details_request>::write_msgpack(
    const ores::reporting::messaging::list_cut_approval_details_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_approval_details_request
wire_traits<ores::reporting::messaging::list_cut_approval_details_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_approval_details_request>(
        data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_approval_details_response>::write_json(
    const ores::reporting::messaging::list_cut_approval_details_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_approval_details_response
wire_traits<ores::reporting::messaging::list_cut_approval_details_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_approval_details_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_approval_details_response>::write_msgpack(
    const ores::reporting::messaging::list_cut_approval_details_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_approval_details_response
wire_traits<ores::reporting::messaging::list_cut_approval_details_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_approval_details_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_approval_detail_request>::write_json(
    const ores::reporting::messaging::get_cut_approval_detail_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_approval_detail_request
wire_traits<ores::reporting::messaging::get_cut_approval_detail_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_approval_detail_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_approval_detail_request>::write_msgpack(
    const ores::reporting::messaging::get_cut_approval_detail_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_approval_detail_request
wire_traits<ores::reporting::messaging::get_cut_approval_detail_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_approval_detail_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_approval_detail_response>::write_json(
    const ores::reporting::messaging::get_cut_approval_detail_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_approval_detail_response
wire_traits<ores::reporting::messaging::get_cut_approval_detail_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_approval_detail_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_cut_approval_detail_response>::write_msgpack(
    const ores::reporting::messaging::get_cut_approval_detail_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_approval_detail_response
wire_traits<ores::reporting::messaging::get_cut_approval_detail_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_approval_detail_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_request>::write_json(
    const ores::reporting::messaging::get_many_cut_approval_details_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_cut_approval_details_request
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_cut_approval_details_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_request>::write_msgpack(
    const ores::reporting::messaging::get_many_cut_approval_details_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_cut_approval_details_request
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_cut_approval_details_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_response>::write_json(
    const ores::reporting::messaging::get_many_cut_approval_details_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_cut_approval_details_response
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_cut_approval_details_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_response>::write_msgpack(
    const ores::reporting::messaging::get_many_cut_approval_details_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_cut_approval_details_response
wire_traits<ores::reporting::messaging::get_many_cut_approval_details_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_cut_approval_details_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_request>::
    write_json(
        const ores::reporting::messaging::list_by_request_id_cut_approval_details_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_by_request_id_cut_approval_details_request
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_by_request_id_cut_approval_details_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_request>::
    write_msgpack(
        const ores::reporting::messaging::list_by_request_id_cut_approval_details_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_by_request_id_cut_approval_details_request
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_by_request_id_cut_approval_details_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_response>::
    write_json(
        const ores::reporting::messaging::list_by_request_id_cut_approval_details_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_by_request_id_cut_approval_details_response
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_by_request_id_cut_approval_details_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_response>::
    write_msgpack(
        const ores::reporting::messaging::list_by_request_id_cut_approval_details_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_by_request_id_cut_approval_details_response
wire_traits<ores::reporting::messaging::list_by_request_id_cut_approval_details_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_by_request_id_cut_approval_details_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_request>::write_json(
    const ores::reporting::messaging::list_by_cut_id_cut_approval_details_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_by_cut_id_cut_approval_details_request
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_by_cut_id_cut_approval_details_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_request>::write_msgpack(
    const ores::reporting::messaging::list_by_cut_id_cut_approval_details_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_by_cut_id_cut_approval_details_request
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_by_cut_id_cut_approval_details_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_response>::write_json(
    const ores::reporting::messaging::list_by_cut_id_cut_approval_details_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_by_cut_id_cut_approval_details_response
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_by_cut_id_cut_approval_details_response>(text);
}

std::string wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_response>::
    write_msgpack(
        const ores::reporting::messaging::list_by_cut_id_cut_approval_details_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_by_cut_id_cut_approval_details_response
wire_traits<ores::reporting::messaging::list_by_cut_id_cut_approval_details_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_by_cut_id_cut_approval_details_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_request>::write_json(
    const ores::reporting::messaging::list_cut_approval_detail_versions_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_approval_detail_versions_request
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_approval_detail_versions_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_request>::write_msgpack(
    const ores::reporting::messaging::list_cut_approval_detail_versions_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_approval_detail_versions_request
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_cut_approval_detail_versions_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_response>::write_json(
    const ores::reporting::messaging::list_cut_approval_detail_versions_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_approval_detail_versions_response
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_cut_approval_detail_versions_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_response>::write_msgpack(
    const ores::reporting::messaging::list_cut_approval_detail_versions_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_approval_detail_versions_response
wire_traits<ores::reporting::messaging::list_cut_approval_detail_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_cut_approval_detail_versions_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_request>::write_json(
    const ores::reporting::messaging::get_cut_approval_detail_version_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_approval_detail_version_request
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_approval_detail_version_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_request>::write_msgpack(
    const ores::reporting::messaging::get_cut_approval_detail_version_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_approval_detail_version_request
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_cut_approval_detail_version_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_response>::write_json(
    const ores::reporting::messaging::get_cut_approval_detail_version_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_approval_detail_version_response
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_approval_detail_version_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_response>::write_msgpack(
    const ores::reporting::messaging::get_cut_approval_detail_version_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_approval_detail_version_response
wire_traits<ores::reporting::messaging::get_cut_approval_detail_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_cut_approval_detail_version_response>(data);
}

}
