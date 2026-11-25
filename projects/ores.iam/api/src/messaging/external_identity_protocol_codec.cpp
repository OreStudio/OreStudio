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
#include "ores.iam.api/domain/external_identity_codec.hpp"
#include "ores.iam.api/messaging/external_identity_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::external_identity_key>::write_json(
    const ores::iam::messaging::external_identity_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_key
wire_traits<ores::iam::messaging::external_identity_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_key>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_key>::write_msgpack(
    const ores::iam::messaging::external_identity_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_key
wire_traits<ores::iam::messaging::external_identity_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_key>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_write>::write_json(
    const ores::iam::messaging::external_identity_write& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_write
wire_traits<ores::iam::messaging::external_identity_write>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_write>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_write>::write_msgpack(
    const ores::iam::messaging::external_identity_write& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_write
wire_traits<ores::iam::messaging::external_identity_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_write>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_change>::write_json(
    const ores::iam::messaging::external_identity_change& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_change
wire_traits<ores::iam::messaging::external_identity_change>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_change>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_change>::write_msgpack(
    const ores::iam::messaging::external_identity_change& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_change
wire_traits<ores::iam::messaging::external_identity_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_change>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_removal>::write_json(
    const ores::iam::messaging::external_identity_removal& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_removal
wire_traits<ores::iam::messaging::external_identity_removal>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_removal>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_removal>::write_msgpack(
    const ores::iam::messaging::external_identity_removal& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_removal
wire_traits<ores::iam::messaging::external_identity_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_removal>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_lookup>::write_json(
    const ores::iam::messaging::external_identity_lookup& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_lookup
wire_traits<ores::iam::messaging::external_identity_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_lookup>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_lookup>::write_msgpack(
    const ores::iam::messaging::external_identity_lookup& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_lookup
wire_traits<ores::iam::messaging::external_identity_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_lookup>(data);
}

std::string wire_traits<ores::iam::messaging::external_identities_filter>::write_json(
    const ores::iam::messaging::external_identities_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identities_filter
wire_traits<ores::iam::messaging::external_identities_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identities_filter>(text);
}

std::string wire_traits<ores::iam::messaging::external_identities_filter>::write_msgpack(
    const ores::iam::messaging::external_identities_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identities_filter
wire_traits<ores::iam::messaging::external_identities_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identities_filter>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_event>::write_json(
    const ores::iam::messaging::external_identity_event& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_event
wire_traits<ores::iam::messaging::external_identity_event>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_event>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_event>::write_msgpack(
    const ores::iam::messaging::external_identity_event& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_event
wire_traits<ores::iam::messaging::external_identity_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_event>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_version_key>::write_json(
    const ores::iam::messaging::external_identity_version_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_version_key
wire_traits<ores::iam::messaging::external_identity_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_version_key>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_version_key>::write_msgpack(
    const ores::iam::messaging::external_identity_version_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_version_key
wire_traits<ores::iam::messaging::external_identity_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_version_key>(data);
}

std::string wire_traits<ores::iam::messaging::external_identity_versions_filter>::write_json(
    const ores::iam::messaging::external_identity_versions_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::external_identity_versions_filter
wire_traits<ores::iam::messaging::external_identity_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::external_identity_versions_filter>(text);
}

std::string wire_traits<ores::iam::messaging::external_identity_versions_filter>::write_msgpack(
    const ores::iam::messaging::external_identity_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::external_identity_versions_filter
wire_traits<ores::iam::messaging::external_identity_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::external_identity_versions_filter>(data);
}

std::string wire_traits<ores::iam::messaging::list_external_identities_request>::write_json(
    const ores::iam::messaging::list_external_identities_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_external_identities_request
wire_traits<ores::iam::messaging::list_external_identities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_external_identities_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_external_identities_request>::write_msgpack(
    const ores::iam::messaging::list_external_identities_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_external_identities_request
wire_traits<ores::iam::messaging::list_external_identities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_external_identities_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_external_identities_response>::write_json(
    const ores::iam::messaging::list_external_identities_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_external_identities_response
wire_traits<ores::iam::messaging::list_external_identities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_external_identities_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_external_identities_response>::write_msgpack(
    const ores::iam::messaging::list_external_identities_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_external_identities_response
wire_traits<ores::iam::messaging::list_external_identities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_external_identities_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_request>::write_json(
    const ores::iam::messaging::get_external_identity_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_external_identity_request
wire_traits<ores::iam::messaging::get_external_identity_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_external_identity_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_request>::write_msgpack(
    const ores::iam::messaging::get_external_identity_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_external_identity_request
wire_traits<ores::iam::messaging::get_external_identity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_external_identity_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_response>::write_json(
    const ores::iam::messaging::get_external_identity_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_external_identity_response
wire_traits<ores::iam::messaging::get_external_identity_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_external_identity_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_response>::write_msgpack(
    const ores::iam::messaging::get_external_identity_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_external_identity_response
wire_traits<ores::iam::messaging::get_external_identity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_external_identity_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_external_identities_request>::write_json(
    const ores::iam::messaging::get_many_external_identities_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_external_identities_request
wire_traits<ores::iam::messaging::get_many_external_identities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_external_identities_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_external_identities_request>::write_msgpack(
    const ores::iam::messaging::get_many_external_identities_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_external_identities_request
wire_traits<ores::iam::messaging::get_many_external_identities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_external_identities_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_external_identities_response>::write_json(
    const ores::iam::messaging::get_many_external_identities_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_external_identities_response
wire_traits<ores::iam::messaging::get_many_external_identities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_external_identities_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_external_identities_response>::write_msgpack(
    const ores::iam::messaging::get_many_external_identities_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_external_identities_response
wire_traits<ores::iam::messaging::get_many_external_identities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_external_identities_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_external_identity_request>::write_json(
    const ores::iam::messaging::put_external_identity_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_external_identity_request
wire_traits<ores::iam::messaging::put_external_identity_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_external_identity_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_external_identity_request>::write_msgpack(
    const ores::iam::messaging::put_external_identity_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_external_identity_request
wire_traits<ores::iam::messaging::put_external_identity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_external_identity_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_external_identity_response>::write_json(
    const ores::iam::messaging::put_external_identity_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_external_identity_response
wire_traits<ores::iam::messaging::put_external_identity_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_external_identity_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_external_identity_response>::write_msgpack(
    const ores::iam::messaging::put_external_identity_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_external_identity_response
wire_traits<ores::iam::messaging::put_external_identity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_external_identity_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_external_identities_request>::write_json(
    const ores::iam::messaging::put_many_external_identities_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_external_identities_request
wire_traits<ores::iam::messaging::put_many_external_identities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_external_identities_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_external_identities_request>::write_msgpack(
    const ores::iam::messaging::put_many_external_identities_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_external_identities_request
wire_traits<ores::iam::messaging::put_many_external_identities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_external_identities_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_external_identities_response>::write_json(
    const ores::iam::messaging::put_many_external_identities_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_external_identities_response
wire_traits<ores::iam::messaging::put_many_external_identities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_external_identities_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_external_identities_response>::write_msgpack(
    const ores::iam::messaging::put_many_external_identities_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_external_identities_response
wire_traits<ores::iam::messaging::put_many_external_identities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_external_identities_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_external_identity_request>::write_json(
    const ores::iam::messaging::delete_external_identity_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_external_identity_request
wire_traits<ores::iam::messaging::delete_external_identity_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_external_identity_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_external_identity_request>::write_msgpack(
    const ores::iam::messaging::delete_external_identity_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_external_identity_request
wire_traits<ores::iam::messaging::delete_external_identity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_external_identity_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_external_identity_response>::write_json(
    const ores::iam::messaging::delete_external_identity_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_external_identity_response
wire_traits<ores::iam::messaging::delete_external_identity_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_external_identity_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_external_identity_response>::write_msgpack(
    const ores::iam::messaging::delete_external_identity_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_external_identity_response
wire_traits<ores::iam::messaging::delete_external_identity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_external_identity_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_external_identities_request>::write_json(
    const ores::iam::messaging::delete_many_external_identities_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_external_identities_request
wire_traits<ores::iam::messaging::delete_many_external_identities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_external_identities_request>(text);
}

std::string
wire_traits<ores::iam::messaging::delete_many_external_identities_request>::write_msgpack(
    const ores::iam::messaging::delete_many_external_identities_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_external_identities_request
wire_traits<ores::iam::messaging::delete_many_external_identities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_external_identities_request>(
        data);
}

std::string wire_traits<ores::iam::messaging::delete_many_external_identities_response>::write_json(
    const ores::iam::messaging::delete_many_external_identities_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_external_identities_response
wire_traits<ores::iam::messaging::delete_many_external_identities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_external_identities_response>(text);
}

std::string
wire_traits<ores::iam::messaging::delete_many_external_identities_response>::write_msgpack(
    const ores::iam::messaging::delete_many_external_identities_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_external_identities_response
wire_traits<ores::iam::messaging::delete_many_external_identities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_external_identities_response>(
        data);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_request>::write_json(
    const ores::iam::messaging::list_by_account_id_external_identities_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_account_id_external_identities_request
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_account_id_external_identities_request>(
        text);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_request>::write_msgpack(
    const ores::iam::messaging::list_by_account_id_external_identities_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_account_id_external_identities_request
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::list_by_account_id_external_identities_request>(data);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_response>::write_json(
    const ores::iam::messaging::list_by_account_id_external_identities_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_account_id_external_identities_response
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_account_id_external_identities_response>(
        text);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_response>::write_msgpack(
    const ores::iam::messaging::list_by_account_id_external_identities_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_account_id_external_identities_response
wire_traits<ores::iam::messaging::list_by_account_id_external_identities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::list_by_account_id_external_identities_response>(data);
}

std::string
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_request>::
    write_json(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_identity_provider_id_external_identities_request
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::iam::messaging::list_by_identity_provider_id_external_identities_request>(text);
}

std::string
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_request>::
    write_msgpack(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_identity_provider_id_external_identities_request
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::list_by_identity_provider_id_external_identities_request>(data);
}

std::string
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_response>::
    write_json(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_identity_provider_id_external_identities_response
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::iam::messaging::list_by_identity_provider_id_external_identities_response>(text);
}

std::string
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_response>::
    write_msgpack(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_identity_provider_id_external_identities_response
wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::list_by_identity_provider_id_external_identities_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_external_identity_versions_request>::write_json(
    const ores::iam::messaging::list_external_identity_versions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_external_identity_versions_request
wire_traits<ores::iam::messaging::list_external_identity_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_external_identity_versions_request>(text);
}

std::string
wire_traits<ores::iam::messaging::list_external_identity_versions_request>::write_msgpack(
    const ores::iam::messaging::list_external_identity_versions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_external_identity_versions_request
wire_traits<ores::iam::messaging::list_external_identity_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_external_identity_versions_request>(
        data);
}

std::string wire_traits<ores::iam::messaging::list_external_identity_versions_response>::write_json(
    const ores::iam::messaging::list_external_identity_versions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_external_identity_versions_response
wire_traits<ores::iam::messaging::list_external_identity_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_external_identity_versions_response>(text);
}

std::string
wire_traits<ores::iam::messaging::list_external_identity_versions_response>::write_msgpack(
    const ores::iam::messaging::list_external_identity_versions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_external_identity_versions_response
wire_traits<ores::iam::messaging::list_external_identity_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_external_identity_versions_response>(
        data);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_version_request>::write_json(
    const ores::iam::messaging::get_external_identity_version_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_external_identity_version_request
wire_traits<ores::iam::messaging::get_external_identity_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_external_identity_version_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_version_request>::write_msgpack(
    const ores::iam::messaging::get_external_identity_version_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_external_identity_version_request
wire_traits<ores::iam::messaging::get_external_identity_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_external_identity_version_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_external_identity_version_response>::write_json(
    const ores::iam::messaging::get_external_identity_version_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_external_identity_version_response
wire_traits<ores::iam::messaging::get_external_identity_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_external_identity_version_response>(text);
}

std::string
wire_traits<ores::iam::messaging::get_external_identity_version_response>::write_msgpack(
    const ores::iam::messaging::get_external_identity_version_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_external_identity_version_response
wire_traits<ores::iam::messaging::get_external_identity_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_external_identity_version_response>(data);
}

}
