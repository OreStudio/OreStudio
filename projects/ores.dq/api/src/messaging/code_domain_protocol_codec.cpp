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
#include "ores.dq.api/domain/code_domain_codec.hpp"
#include "ores.dq.api/messaging/code_domain_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::code_domain_key>::write_json(
    const ores::dq::messaging::code_domain_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_key
wire_traits<ores::dq::messaging::code_domain_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_key>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_key>::write_msgpack(
    const ores::dq::messaging::code_domain_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_key
wire_traits<ores::dq::messaging::code_domain_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_key>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_write>::write_json(
    const ores::dq::messaging::code_domain_write& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_write
wire_traits<ores::dq::messaging::code_domain_write>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_write>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_write>::write_msgpack(
    const ores::dq::messaging::code_domain_write& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_write
wire_traits<ores::dq::messaging::code_domain_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_write>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_change>::write_json(
    const ores::dq::messaging::code_domain_change& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_change
wire_traits<ores::dq::messaging::code_domain_change>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_change>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_change>::write_msgpack(
    const ores::dq::messaging::code_domain_change& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_change
wire_traits<ores::dq::messaging::code_domain_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_change>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_removal>::write_json(
    const ores::dq::messaging::code_domain_removal& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_removal
wire_traits<ores::dq::messaging::code_domain_removal>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_removal>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_removal>::write_msgpack(
    const ores::dq::messaging::code_domain_removal& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_removal
wire_traits<ores::dq::messaging::code_domain_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_removal>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_lookup>::write_json(
    const ores::dq::messaging::code_domain_lookup& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_lookup
wire_traits<ores::dq::messaging::code_domain_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_lookup>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_lookup>::write_msgpack(
    const ores::dq::messaging::code_domain_lookup& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_lookup
wire_traits<ores::dq::messaging::code_domain_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_lookup>(data);
}

std::string wire_traits<ores::dq::messaging::code_domains_filter>::write_json(
    const ores::dq::messaging::code_domains_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domains_filter
wire_traits<ores::dq::messaging::code_domains_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domains_filter>(text);
}

std::string wire_traits<ores::dq::messaging::code_domains_filter>::write_msgpack(
    const ores::dq::messaging::code_domains_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domains_filter
wire_traits<ores::dq::messaging::code_domains_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domains_filter>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_event>::write_json(
    const ores::dq::messaging::code_domain_event& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_event
wire_traits<ores::dq::messaging::code_domain_event>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_event>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_event>::write_msgpack(
    const ores::dq::messaging::code_domain_event& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_event
wire_traits<ores::dq::messaging::code_domain_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_event>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_version_key>::write_json(
    const ores::dq::messaging::code_domain_version_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_version_key
wire_traits<ores::dq::messaging::code_domain_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_version_key>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_version_key>::write_msgpack(
    const ores::dq::messaging::code_domain_version_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_version_key
wire_traits<ores::dq::messaging::code_domain_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_version_key>(data);
}

std::string wire_traits<ores::dq::messaging::code_domain_versions_filter>::write_json(
    const ores::dq::messaging::code_domain_versions_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::code_domain_versions_filter
wire_traits<ores::dq::messaging::code_domain_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::code_domain_versions_filter>(text);
}

std::string wire_traits<ores::dq::messaging::code_domain_versions_filter>::write_msgpack(
    const ores::dq::messaging::code_domain_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::code_domain_versions_filter
wire_traits<ores::dq::messaging::code_domain_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::code_domain_versions_filter>(data);
}

std::string wire_traits<ores::dq::messaging::list_code_domains_request>::write_json(
    const ores::dq::messaging::list_code_domains_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_code_domains_request
wire_traits<ores::dq::messaging::list_code_domains_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_code_domains_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_code_domains_request>::write_msgpack(
    const ores::dq::messaging::list_code_domains_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_code_domains_request
wire_traits<ores::dq::messaging::list_code_domains_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_code_domains_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_code_domains_response>::write_json(
    const ores::dq::messaging::list_code_domains_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_code_domains_response
wire_traits<ores::dq::messaging::list_code_domains_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_code_domains_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_code_domains_response>::write_msgpack(
    const ores::dq::messaging::list_code_domains_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_code_domains_response
wire_traits<ores::dq::messaging::list_code_domains_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_code_domains_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_request>::write_json(
    const ores::dq::messaging::get_code_domain_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_code_domain_request
wire_traits<ores::dq::messaging::get_code_domain_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_code_domain_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_request>::write_msgpack(
    const ores::dq::messaging::get_code_domain_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_code_domain_request
wire_traits<ores::dq::messaging::get_code_domain_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_code_domain_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_response>::write_json(
    const ores::dq::messaging::get_code_domain_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_code_domain_response
wire_traits<ores::dq::messaging::get_code_domain_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_code_domain_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_response>::write_msgpack(
    const ores::dq::messaging::get_code_domain_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_code_domain_response
wire_traits<ores::dq::messaging::get_code_domain_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_code_domain_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_code_domains_request>::write_json(
    const ores::dq::messaging::get_many_code_domains_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_code_domains_request
wire_traits<ores::dq::messaging::get_many_code_domains_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_code_domains_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_code_domains_request>::write_msgpack(
    const ores::dq::messaging::get_many_code_domains_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_code_domains_request
wire_traits<ores::dq::messaging::get_many_code_domains_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_code_domains_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_code_domains_response>::write_json(
    const ores::dq::messaging::get_many_code_domains_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_code_domains_response
wire_traits<ores::dq::messaging::get_many_code_domains_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_code_domains_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_code_domains_response>::write_msgpack(
    const ores::dq::messaging::get_many_code_domains_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_code_domains_response
wire_traits<ores::dq::messaging::get_many_code_domains_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_code_domains_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_code_domain_request>::write_json(
    const ores::dq::messaging::put_code_domain_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_code_domain_request
wire_traits<ores::dq::messaging::put_code_domain_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_code_domain_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_code_domain_request>::write_msgpack(
    const ores::dq::messaging::put_code_domain_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_code_domain_request
wire_traits<ores::dq::messaging::put_code_domain_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_code_domain_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_code_domain_response>::write_json(
    const ores::dq::messaging::put_code_domain_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_code_domain_response
wire_traits<ores::dq::messaging::put_code_domain_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_code_domain_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_code_domain_response>::write_msgpack(
    const ores::dq::messaging::put_code_domain_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_code_domain_response
wire_traits<ores::dq::messaging::put_code_domain_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_code_domain_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_code_domains_request>::write_json(
    const ores::dq::messaging::put_many_code_domains_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_code_domains_request
wire_traits<ores::dq::messaging::put_many_code_domains_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_code_domains_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_code_domains_request>::write_msgpack(
    const ores::dq::messaging::put_many_code_domains_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_code_domains_request
wire_traits<ores::dq::messaging::put_many_code_domains_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_code_domains_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_code_domains_response>::write_json(
    const ores::dq::messaging::put_many_code_domains_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_code_domains_response
wire_traits<ores::dq::messaging::put_many_code_domains_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_code_domains_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_code_domains_response>::write_msgpack(
    const ores::dq::messaging::put_many_code_domains_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_code_domains_response
wire_traits<ores::dq::messaging::put_many_code_domains_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_code_domains_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_code_domain_request>::write_json(
    const ores::dq::messaging::delete_code_domain_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_code_domain_request
wire_traits<ores::dq::messaging::delete_code_domain_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_code_domain_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_code_domain_request>::write_msgpack(
    const ores::dq::messaging::delete_code_domain_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_code_domain_request
wire_traits<ores::dq::messaging::delete_code_domain_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_code_domain_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_code_domain_response>::write_json(
    const ores::dq::messaging::delete_code_domain_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_code_domain_response
wire_traits<ores::dq::messaging::delete_code_domain_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_code_domain_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_code_domain_response>::write_msgpack(
    const ores::dq::messaging::delete_code_domain_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_code_domain_response
wire_traits<ores::dq::messaging::delete_code_domain_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_code_domain_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_code_domains_request>::write_json(
    const ores::dq::messaging::delete_many_code_domains_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_code_domains_request
wire_traits<ores::dq::messaging::delete_many_code_domains_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_code_domains_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_code_domains_request>::write_msgpack(
    const ores::dq::messaging::delete_many_code_domains_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_code_domains_request
wire_traits<ores::dq::messaging::delete_many_code_domains_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_code_domains_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_code_domains_response>::write_json(
    const ores::dq::messaging::delete_many_code_domains_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_code_domains_response
wire_traits<ores::dq::messaging::delete_many_code_domains_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_code_domains_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_code_domains_response>::write_msgpack(
    const ores::dq::messaging::delete_many_code_domains_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_code_domains_response
wire_traits<ores::dq::messaging::delete_many_code_domains_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_code_domains_response>(data);
}

std::string wire_traits<ores::dq::messaging::list_code_domain_versions_request>::write_json(
    const ores::dq::messaging::list_code_domain_versions_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_code_domain_versions_request
wire_traits<ores::dq::messaging::list_code_domain_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_code_domain_versions_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_code_domain_versions_request>::write_msgpack(
    const ores::dq::messaging::list_code_domain_versions_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_code_domain_versions_request
wire_traits<ores::dq::messaging::list_code_domain_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_code_domain_versions_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_code_domain_versions_response>::write_json(
    const ores::dq::messaging::list_code_domain_versions_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_code_domain_versions_response
wire_traits<ores::dq::messaging::list_code_domain_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_code_domain_versions_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_code_domain_versions_response>::write_msgpack(
    const ores::dq::messaging::list_code_domain_versions_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_code_domain_versions_response
wire_traits<ores::dq::messaging::list_code_domain_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_code_domain_versions_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_version_request>::write_json(
    const ores::dq::messaging::get_code_domain_version_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_code_domain_version_request
wire_traits<ores::dq::messaging::get_code_domain_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_code_domain_version_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_version_request>::write_msgpack(
    const ores::dq::messaging::get_code_domain_version_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_code_domain_version_request
wire_traits<ores::dq::messaging::get_code_domain_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_code_domain_version_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_version_response>::write_json(
    const ores::dq::messaging::get_code_domain_version_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_code_domain_version_response
wire_traits<ores::dq::messaging::get_code_domain_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_code_domain_version_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_code_domain_version_response>::write_msgpack(
    const ores::dq::messaging::get_code_domain_version_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_code_domain_version_response
wire_traits<ores::dq::messaging::get_code_domain_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_code_domain_version_response>(data);
}

}
