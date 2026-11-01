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
#include "ores.trading.api/domain/bond_repo_codec.hpp"
#include "ores.trading.api/messaging/bond_repo_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::bond_repo_key>::write_json(
    const ores::trading::messaging::bond_repo_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_key
wire_traits<ores::trading::messaging::bond_repo_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_key>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_key>::write_msgpack(
    const ores::trading::messaging::bond_repo_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_key
wire_traits<ores::trading::messaging::bond_repo_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_key>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_write>::write_json(
    const ores::trading::messaging::bond_repo_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_write
wire_traits<ores::trading::messaging::bond_repo_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_write>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_write>::write_msgpack(
    const ores::trading::messaging::bond_repo_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_write
wire_traits<ores::trading::messaging::bond_repo_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_write>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_change>::write_json(
    const ores::trading::messaging::bond_repo_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_change
wire_traits<ores::trading::messaging::bond_repo_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_change>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_change>::write_msgpack(
    const ores::trading::messaging::bond_repo_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_change
wire_traits<ores::trading::messaging::bond_repo_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_change>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_removal>::write_json(
    const ores::trading::messaging::bond_repo_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_removal
wire_traits<ores::trading::messaging::bond_repo_removal>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_removal>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_removal>::write_msgpack(
    const ores::trading::messaging::bond_repo_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_removal
wire_traits<ores::trading::messaging::bond_repo_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_removal>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_lookup>::write_json(
    const ores::trading::messaging::bond_repo_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_lookup
wire_traits<ores::trading::messaging::bond_repo_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_lookup>::write_msgpack(
    const ores::trading::messaging::bond_repo_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_lookup
wire_traits<ores::trading::messaging::bond_repo_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repos_filter>::write_json(
    const ores::trading::messaging::bond_repos_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repos_filter
wire_traits<ores::trading::messaging::bond_repos_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repos_filter>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repos_filter>::write_msgpack(
    const ores::trading::messaging::bond_repos_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repos_filter
wire_traits<ores::trading::messaging::bond_repos_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repos_filter>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_event>::write_json(
    const ores::trading::messaging::bond_repo_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_event
wire_traits<ores::trading::messaging::bond_repo_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_event>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_event>::write_msgpack(
    const ores::trading::messaging::bond_repo_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_event
wire_traits<ores::trading::messaging::bond_repo_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_event>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_version_key>::write_json(
    const ores::trading::messaging::bond_repo_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_version_key
wire_traits<ores::trading::messaging::bond_repo_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_version_key>::write_msgpack(
    const ores::trading::messaging::bond_repo_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_version_key
wire_traits<ores::trading::messaging::bond_repo_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::bond_repo_versions_filter>::write_json(
    const ores::trading::messaging::bond_repo_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_repo_versions_filter
wire_traits<ores::trading::messaging::bond_repo_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_repo_versions_filter>(text);
}

std::string wire_traits<ores::trading::messaging::bond_repo_versions_filter>::write_msgpack(
    const ores::trading::messaging::bond_repo_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_repo_versions_filter
wire_traits<ores::trading::messaging::bond_repo_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_repo_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_bond_repos_request>::write_json(
    const ores::trading::messaging::list_bond_repos_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_repos_request
wire_traits<ores::trading::messaging::list_bond_repos_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_repos_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_bond_repos_request>::write_msgpack(
    const ores::trading::messaging::list_bond_repos_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_repos_request
wire_traits<ores::trading::messaging::list_bond_repos_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_repos_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_bond_repos_response>::write_json(
    const ores::trading::messaging::list_bond_repos_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_repos_response
wire_traits<ores::trading::messaging::list_bond_repos_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_repos_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_bond_repos_response>::write_msgpack(
    const ores::trading::messaging::list_bond_repos_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_repos_response
wire_traits<ores::trading::messaging::list_bond_repos_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_repos_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_request>::write_json(
    const ores::trading::messaging::get_bond_repo_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_repo_request
wire_traits<ores::trading::messaging::get_bond_repo_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_repo_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_request>::write_msgpack(
    const ores::trading::messaging::get_bond_repo_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_repo_request
wire_traits<ores::trading::messaging::get_bond_repo_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_repo_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_response>::write_json(
    const ores::trading::messaging::get_bond_repo_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_repo_response
wire_traits<ores::trading::messaging::get_bond_repo_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_repo_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_response>::write_msgpack(
    const ores::trading::messaging::get_bond_repo_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_repo_response
wire_traits<ores::trading::messaging::get_bond_repo_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_repo_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_repos_request>::write_json(
    const ores::trading::messaging::get_many_bond_repos_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_bond_repos_request
wire_traits<ores::trading::messaging::get_many_bond_repos_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_bond_repos_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_repos_request>::write_msgpack(
    const ores::trading::messaging::get_many_bond_repos_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_bond_repos_request
wire_traits<ores::trading::messaging::get_many_bond_repos_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_bond_repos_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_repos_response>::write_json(
    const ores::trading::messaging::get_many_bond_repos_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_bond_repos_response
wire_traits<ores::trading::messaging::get_many_bond_repos_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_bond_repos_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_repos_response>::write_msgpack(
    const ores::trading::messaging::get_many_bond_repos_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_bond_repos_response
wire_traits<ores::trading::messaging::get_many_bond_repos_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_bond_repos_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_bond_repo_request>::write_json(
    const ores::trading::messaging::put_bond_repo_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_bond_repo_request
wire_traits<ores::trading::messaging::put_bond_repo_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_bond_repo_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_bond_repo_request>::write_msgpack(
    const ores::trading::messaging::put_bond_repo_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_bond_repo_request
wire_traits<ores::trading::messaging::put_bond_repo_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_bond_repo_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_bond_repo_response>::write_json(
    const ores::trading::messaging::put_bond_repo_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_bond_repo_response
wire_traits<ores::trading::messaging::put_bond_repo_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_bond_repo_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_bond_repo_response>::write_msgpack(
    const ores::trading::messaging::put_bond_repo_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_bond_repo_response
wire_traits<ores::trading::messaging::put_bond_repo_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_bond_repo_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_repos_request>::write_json(
    const ores::trading::messaging::put_many_bond_repos_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_bond_repos_request
wire_traits<ores::trading::messaging::put_many_bond_repos_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_bond_repos_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_repos_request>::write_msgpack(
    const ores::trading::messaging::put_many_bond_repos_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_bond_repos_request
wire_traits<ores::trading::messaging::put_many_bond_repos_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_bond_repos_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_repos_response>::write_json(
    const ores::trading::messaging::put_many_bond_repos_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_bond_repos_response
wire_traits<ores::trading::messaging::put_many_bond_repos_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_bond_repos_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_repos_response>::write_msgpack(
    const ores::trading::messaging::put_many_bond_repos_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_bond_repos_response
wire_traits<ores::trading::messaging::put_many_bond_repos_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_bond_repos_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_bond_repo_request>::write_json(
    const ores::trading::messaging::delete_bond_repo_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_bond_repo_request
wire_traits<ores::trading::messaging::delete_bond_repo_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_bond_repo_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_bond_repo_request>::write_msgpack(
    const ores::trading::messaging::delete_bond_repo_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_bond_repo_request
wire_traits<ores::trading::messaging::delete_bond_repo_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_bond_repo_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_bond_repo_response>::write_json(
    const ores::trading::messaging::delete_bond_repo_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_bond_repo_response
wire_traits<ores::trading::messaging::delete_bond_repo_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_bond_repo_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_bond_repo_response>::write_msgpack(
    const ores::trading::messaging::delete_bond_repo_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_bond_repo_response
wire_traits<ores::trading::messaging::delete_bond_repo_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_bond_repo_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_bond_repos_request>::write_json(
    const ores::trading::messaging::delete_many_bond_repos_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_bond_repos_request
wire_traits<ores::trading::messaging::delete_many_bond_repos_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_bond_repos_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_many_bond_repos_request>::write_msgpack(
    const ores::trading::messaging::delete_many_bond_repos_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_bond_repos_request
wire_traits<ores::trading::messaging::delete_many_bond_repos_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_bond_repos_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_bond_repos_response>::write_json(
    const ores::trading::messaging::delete_many_bond_repos_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_bond_repos_response
wire_traits<ores::trading::messaging::delete_many_bond_repos_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_bond_repos_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_many_bond_repos_response>::write_msgpack(
    const ores::trading::messaging::delete_many_bond_repos_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_bond_repos_response
wire_traits<ores::trading::messaging::delete_many_bond_repos_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_bond_repos_response>(data);
}

std::string wire_traits<ores::trading::messaging::list_bond_repo_versions_request>::write_json(
    const ores::trading::messaging::list_bond_repo_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_repo_versions_request
wire_traits<ores::trading::messaging::list_bond_repo_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_repo_versions_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_bond_repo_versions_request>::write_msgpack(
    const ores::trading::messaging::list_bond_repo_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_repo_versions_request
wire_traits<ores::trading::messaging::list_bond_repo_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_repo_versions_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_bond_repo_versions_response>::write_json(
    const ores::trading::messaging::list_bond_repo_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_repo_versions_response
wire_traits<ores::trading::messaging::list_bond_repo_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_repo_versions_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_bond_repo_versions_response>::write_msgpack(
    const ores::trading::messaging::list_bond_repo_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_repo_versions_response
wire_traits<ores::trading::messaging::list_bond_repo_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_repo_versions_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_version_request>::write_json(
    const ores::trading::messaging::get_bond_repo_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_repo_version_request
wire_traits<ores::trading::messaging::get_bond_repo_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_repo_version_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_version_request>::write_msgpack(
    const ores::trading::messaging::get_bond_repo_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_repo_version_request
wire_traits<ores::trading::messaging::get_bond_repo_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_repo_version_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_version_response>::write_json(
    const ores::trading::messaging::get_bond_repo_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_repo_version_response
wire_traits<ores::trading::messaging::get_bond_repo_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_repo_version_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_repo_version_response>::write_msgpack(
    const ores::trading::messaging::get_bond_repo_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_repo_version_response
wire_traits<ores::trading::messaging::get_bond_repo_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_repo_version_response>(data);
}

}
