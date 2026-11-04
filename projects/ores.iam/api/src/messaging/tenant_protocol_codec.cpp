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
#include "ores.iam.api/domain/tenant_codec.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::tenant_key>::write_json(
    const ores::iam::messaging::tenant_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_key
wire_traits<ores::iam::messaging::tenant_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_key>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_key>::write_msgpack(
    const ores::iam::messaging::tenant_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_key
wire_traits<ores::iam::messaging::tenant_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_key>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_write>::write_json(
    const ores::iam::messaging::tenant_write& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_write
wire_traits<ores::iam::messaging::tenant_write>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_write>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_write>::write_msgpack(
    const ores::iam::messaging::tenant_write& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_write
wire_traits<ores::iam::messaging::tenant_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_write>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_change>::write_json(
    const ores::iam::messaging::tenant_change& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_change
wire_traits<ores::iam::messaging::tenant_change>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_change>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_change>::write_msgpack(
    const ores::iam::messaging::tenant_change& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_change
wire_traits<ores::iam::messaging::tenant_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_change>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_removal>::write_json(
    const ores::iam::messaging::tenant_removal& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_removal
wire_traits<ores::iam::messaging::tenant_removal>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_removal>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_removal>::write_msgpack(
    const ores::iam::messaging::tenant_removal& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_removal
wire_traits<ores::iam::messaging::tenant_removal>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_removal>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_lookup>::write_json(
    const ores::iam::messaging::tenant_lookup& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_lookup
wire_traits<ores::iam::messaging::tenant_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_lookup>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_lookup>::write_msgpack(
    const ores::iam::messaging::tenant_lookup& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_lookup
wire_traits<ores::iam::messaging::tenant_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_lookup>(data);
}

std::string wire_traits<ores::iam::messaging::tenants_filter>::write_json(
    const ores::iam::messaging::tenants_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenants_filter
wire_traits<ores::iam::messaging::tenants_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenants_filter>(text);
}

std::string wire_traits<ores::iam::messaging::tenants_filter>::write_msgpack(
    const ores::iam::messaging::tenants_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenants_filter
wire_traits<ores::iam::messaging::tenants_filter>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenants_filter>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_event>::write_json(
    const ores::iam::messaging::tenant_event& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_event
wire_traits<ores::iam::messaging::tenant_event>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_event>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_event>::write_msgpack(
    const ores::iam::messaging::tenant_event& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_event
wire_traits<ores::iam::messaging::tenant_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_event>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_version_key>::write_json(
    const ores::iam::messaging::tenant_version_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_version_key
wire_traits<ores::iam::messaging::tenant_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_version_key>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_version_key>::write_msgpack(
    const ores::iam::messaging::tenant_version_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_version_key
wire_traits<ores::iam::messaging::tenant_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_version_key>(data);
}

std::string wire_traits<ores::iam::messaging::tenant_versions_filter>::write_json(
    const ores::iam::messaging::tenant_versions_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::tenant_versions_filter
wire_traits<ores::iam::messaging::tenant_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::tenant_versions_filter>(text);
}

std::string wire_traits<ores::iam::messaging::tenant_versions_filter>::write_msgpack(
    const ores::iam::messaging::tenant_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::tenant_versions_filter
wire_traits<ores::iam::messaging::tenant_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::tenant_versions_filter>(data);
}

std::string wire_traits<ores::iam::messaging::list_tenants_request>::write_json(
    const ores::iam::messaging::list_tenants_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_tenants_request
wire_traits<ores::iam::messaging::list_tenants_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_tenants_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_tenants_request>::write_msgpack(
    const ores::iam::messaging::list_tenants_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_tenants_request
wire_traits<ores::iam::messaging::list_tenants_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_tenants_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_tenants_response>::write_json(
    const ores::iam::messaging::list_tenants_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_tenants_response
wire_traits<ores::iam::messaging::list_tenants_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_tenants_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_tenants_response>::write_msgpack(
    const ores::iam::messaging::list_tenants_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_tenants_response
wire_traits<ores::iam::messaging::list_tenants_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_tenants_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_tenant_request>::write_json(
    const ores::iam::messaging::get_tenant_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_tenant_request
wire_traits<ores::iam::messaging::get_tenant_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_tenant_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_tenant_request>::write_msgpack(
    const ores::iam::messaging::get_tenant_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_tenant_request
wire_traits<ores::iam::messaging::get_tenant_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_tenant_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_tenant_response>::write_json(
    const ores::iam::messaging::get_tenant_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_tenant_response
wire_traits<ores::iam::messaging::get_tenant_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_tenant_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_tenant_response>::write_msgpack(
    const ores::iam::messaging::get_tenant_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_tenant_response
wire_traits<ores::iam::messaging::get_tenant_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_tenant_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_tenants_request>::write_json(
    const ores::iam::messaging::get_many_tenants_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_tenants_request
wire_traits<ores::iam::messaging::get_many_tenants_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_tenants_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_tenants_request>::write_msgpack(
    const ores::iam::messaging::get_many_tenants_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_tenants_request
wire_traits<ores::iam::messaging::get_many_tenants_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_tenants_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_tenants_response>::write_json(
    const ores::iam::messaging::get_many_tenants_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_tenants_response
wire_traits<ores::iam::messaging::get_many_tenants_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_tenants_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_tenants_response>::write_msgpack(
    const ores::iam::messaging::get_many_tenants_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_tenants_response
wire_traits<ores::iam::messaging::get_many_tenants_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_tenants_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_tenant_request>::write_json(
    const ores::iam::messaging::put_tenant_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_tenant_request
wire_traits<ores::iam::messaging::put_tenant_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_tenant_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_tenant_request>::write_msgpack(
    const ores::iam::messaging::put_tenant_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_tenant_request
wire_traits<ores::iam::messaging::put_tenant_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_tenant_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_tenant_response>::write_json(
    const ores::iam::messaging::put_tenant_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_tenant_response
wire_traits<ores::iam::messaging::put_tenant_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_tenant_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_tenant_response>::write_msgpack(
    const ores::iam::messaging::put_tenant_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_tenant_response
wire_traits<ores::iam::messaging::put_tenant_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_tenant_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_tenants_request>::write_json(
    const ores::iam::messaging::put_many_tenants_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_tenants_request
wire_traits<ores::iam::messaging::put_many_tenants_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_tenants_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_tenants_request>::write_msgpack(
    const ores::iam::messaging::put_many_tenants_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_tenants_request
wire_traits<ores::iam::messaging::put_many_tenants_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_tenants_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_tenants_response>::write_json(
    const ores::iam::messaging::put_many_tenants_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_tenants_response
wire_traits<ores::iam::messaging::put_many_tenants_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_tenants_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_tenants_response>::write_msgpack(
    const ores::iam::messaging::put_many_tenants_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_tenants_response
wire_traits<ores::iam::messaging::put_many_tenants_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_tenants_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_tenant_request>::write_json(
    const ores::iam::messaging::delete_tenant_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_tenant_request
wire_traits<ores::iam::messaging::delete_tenant_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_tenant_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_tenant_request>::write_msgpack(
    const ores::iam::messaging::delete_tenant_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_tenant_request
wire_traits<ores::iam::messaging::delete_tenant_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_tenant_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_tenant_response>::write_json(
    const ores::iam::messaging::delete_tenant_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_tenant_response
wire_traits<ores::iam::messaging::delete_tenant_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_tenant_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_tenant_response>::write_msgpack(
    const ores::iam::messaging::delete_tenant_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_tenant_response
wire_traits<ores::iam::messaging::delete_tenant_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_tenant_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_tenants_request>::write_json(
    const ores::iam::messaging::delete_many_tenants_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_tenants_request
wire_traits<ores::iam::messaging::delete_many_tenants_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_tenants_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_tenants_request>::write_msgpack(
    const ores::iam::messaging::delete_many_tenants_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_tenants_request
wire_traits<ores::iam::messaging::delete_many_tenants_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_tenants_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_tenants_response>::write_json(
    const ores::iam::messaging::delete_many_tenants_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_tenants_response
wire_traits<ores::iam::messaging::delete_many_tenants_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_tenants_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_tenants_response>::write_msgpack(
    const ores::iam::messaging::delete_many_tenants_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_tenants_response
wire_traits<ores::iam::messaging::delete_many_tenants_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_tenants_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_tenant_versions_request>::write_json(
    const ores::iam::messaging::list_tenant_versions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_tenant_versions_request
wire_traits<ores::iam::messaging::list_tenant_versions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_tenant_versions_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_tenant_versions_request>::write_msgpack(
    const ores::iam::messaging::list_tenant_versions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_tenant_versions_request
wire_traits<ores::iam::messaging::list_tenant_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_tenant_versions_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_tenant_versions_response>::write_json(
    const ores::iam::messaging::list_tenant_versions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_tenant_versions_response
wire_traits<ores::iam::messaging::list_tenant_versions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_tenant_versions_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_tenant_versions_response>::write_msgpack(
    const ores::iam::messaging::list_tenant_versions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_tenant_versions_response
wire_traits<ores::iam::messaging::list_tenant_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_tenant_versions_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_tenant_version_request>::write_json(
    const ores::iam::messaging::get_tenant_version_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_tenant_version_request
wire_traits<ores::iam::messaging::get_tenant_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_tenant_version_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_tenant_version_request>::write_msgpack(
    const ores::iam::messaging::get_tenant_version_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_tenant_version_request
wire_traits<ores::iam::messaging::get_tenant_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_tenant_version_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_tenant_version_response>::write_json(
    const ores::iam::messaging::get_tenant_version_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_tenant_version_response
wire_traits<ores::iam::messaging::get_tenant_version_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_tenant_version_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_tenant_version_response>::write_msgpack(
    const ores::iam::messaging::get_tenant_version_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_tenant_version_response
wire_traits<ores::iam::messaging::get_tenant_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_tenant_version_response>(data);
}

}
