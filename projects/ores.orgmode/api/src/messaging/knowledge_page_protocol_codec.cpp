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
#include "ores.orgmode.api/domain/knowledge_page_codec.hpp"
#include "ores.orgmode.api/messaging/knowledge_page_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::orgmode::messaging::knowledge_page_key>::write_json(
    const ores::orgmode::messaging::knowledge_page_key& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::knowledge_page_key
wire_traits<ores::orgmode::messaging::knowledge_page_key>::read_json(std::string_view text) {
    return object::read_json<ores::orgmode::messaging::knowledge_page_key>(text);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_key>::write_msgpack(
    const ores::orgmode::messaging::knowledge_page_key& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::knowledge_page_key
wire_traits<ores::orgmode::messaging::knowledge_page_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::knowledge_page_key>(data);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_lookup>::write_json(
    const ores::orgmode::messaging::knowledge_page_lookup& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::knowledge_page_lookup
wire_traits<ores::orgmode::messaging::knowledge_page_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::orgmode::messaging::knowledge_page_lookup>(text);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_lookup>::write_msgpack(
    const ores::orgmode::messaging::knowledge_page_lookup& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::knowledge_page_lookup
wire_traits<ores::orgmode::messaging::knowledge_page_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::knowledge_page_lookup>(data);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_pages_filter>::write_json(
    const ores::orgmode::messaging::knowledge_pages_filter& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::knowledge_pages_filter
wire_traits<ores::orgmode::messaging::knowledge_pages_filter>::read_json(std::string_view text) {
    return object::read_json<ores::orgmode::messaging::knowledge_pages_filter>(text);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_pages_filter>::write_msgpack(
    const ores::orgmode::messaging::knowledge_pages_filter& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::knowledge_pages_filter
wire_traits<ores::orgmode::messaging::knowledge_pages_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::knowledge_pages_filter>(data);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_event>::write_json(
    const ores::orgmode::messaging::knowledge_page_event& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::knowledge_page_event
wire_traits<ores::orgmode::messaging::knowledge_page_event>::read_json(std::string_view text) {
    return object::read_json<ores::orgmode::messaging::knowledge_page_event>(text);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_event>::write_msgpack(
    const ores::orgmode::messaging::knowledge_page_event& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::knowledge_page_event
wire_traits<ores::orgmode::messaging::knowledge_page_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::knowledge_page_event>(data);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_version_key>::write_json(
    const ores::orgmode::messaging::knowledge_page_version_key& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::knowledge_page_version_key
wire_traits<ores::orgmode::messaging::knowledge_page_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::knowledge_page_version_key>(text);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_version_key>::write_msgpack(
    const ores::orgmode::messaging::knowledge_page_version_key& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::knowledge_page_version_key
wire_traits<ores::orgmode::messaging::knowledge_page_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::knowledge_page_version_key>(data);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_versions_filter>::write_json(
    const ores::orgmode::messaging::knowledge_page_versions_filter& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::knowledge_page_versions_filter
wire_traits<ores::orgmode::messaging::knowledge_page_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::knowledge_page_versions_filter>(text);
}

std::string wire_traits<ores::orgmode::messaging::knowledge_page_versions_filter>::write_msgpack(
    const ores::orgmode::messaging::knowledge_page_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::knowledge_page_versions_filter
wire_traits<ores::orgmode::messaging::knowledge_page_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::knowledge_page_versions_filter>(data);
}

std::string wire_traits<ores::orgmode::messaging::list_knowledge_pages_request>::write_json(
    const ores::orgmode::messaging::list_knowledge_pages_request& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::list_knowledge_pages_request
wire_traits<ores::orgmode::messaging::list_knowledge_pages_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::list_knowledge_pages_request>(text);
}

std::string wire_traits<ores::orgmode::messaging::list_knowledge_pages_request>::write_msgpack(
    const ores::orgmode::messaging::list_knowledge_pages_request& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::list_knowledge_pages_request
wire_traits<ores::orgmode::messaging::list_knowledge_pages_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::list_knowledge_pages_request>(data);
}

std::string wire_traits<ores::orgmode::messaging::list_knowledge_pages_response>::write_json(
    const ores::orgmode::messaging::list_knowledge_pages_response& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::list_knowledge_pages_response
wire_traits<ores::orgmode::messaging::list_knowledge_pages_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::list_knowledge_pages_response>(text);
}

std::string wire_traits<ores::orgmode::messaging::list_knowledge_pages_response>::write_msgpack(
    const ores::orgmode::messaging::list_knowledge_pages_response& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::list_knowledge_pages_response
wire_traits<ores::orgmode::messaging::list_knowledge_pages_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::list_knowledge_pages_response>(data);
}

std::string wire_traits<ores::orgmode::messaging::get_knowledge_page_request>::write_json(
    const ores::orgmode::messaging::get_knowledge_page_request& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::get_knowledge_page_request
wire_traits<ores::orgmode::messaging::get_knowledge_page_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::get_knowledge_page_request>(text);
}

std::string wire_traits<ores::orgmode::messaging::get_knowledge_page_request>::write_msgpack(
    const ores::orgmode::messaging::get_knowledge_page_request& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::get_knowledge_page_request
wire_traits<ores::orgmode::messaging::get_knowledge_page_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::get_knowledge_page_request>(data);
}

std::string wire_traits<ores::orgmode::messaging::get_knowledge_page_response>::write_json(
    const ores::orgmode::messaging::get_knowledge_page_response& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::get_knowledge_page_response
wire_traits<ores::orgmode::messaging::get_knowledge_page_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::get_knowledge_page_response>(text);
}

std::string wire_traits<ores::orgmode::messaging::get_knowledge_page_response>::write_msgpack(
    const ores::orgmode::messaging::get_knowledge_page_response& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::get_knowledge_page_response
wire_traits<ores::orgmode::messaging::get_knowledge_page_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::get_knowledge_page_response>(data);
}

std::string wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_request>::write_json(
    const ores::orgmode::messaging::get_many_knowledge_pages_request& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::get_many_knowledge_pages_request
wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::get_many_knowledge_pages_request>(text);
}

std::string wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_request>::write_msgpack(
    const ores::orgmode::messaging::get_many_knowledge_pages_request& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::get_many_knowledge_pages_request
wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::get_many_knowledge_pages_request>(data);
}

std::string wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_response>::write_json(
    const ores::orgmode::messaging::get_many_knowledge_pages_response& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::get_many_knowledge_pages_response
wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::get_many_knowledge_pages_response>(text);
}

std::string wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_response>::write_msgpack(
    const ores::orgmode::messaging::get_many_knowledge_pages_response& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::get_many_knowledge_pages_response
wire_traits<ores::orgmode::messaging::get_many_knowledge_pages_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::get_many_knowledge_pages_response>(data);
}

std::string wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_request>::write_json(
    const ores::orgmode::messaging::list_knowledge_page_versions_request& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::list_knowledge_page_versions_request
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::list_knowledge_page_versions_request>(text);
}

std::string
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_request>::write_msgpack(
    const ores::orgmode::messaging::list_knowledge_page_versions_request& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::list_knowledge_page_versions_request
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::list_knowledge_page_versions_request>(
        data);
}

std::string
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_response>::write_json(
    const ores::orgmode::messaging::list_knowledge_page_versions_response& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::list_knowledge_page_versions_response
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::list_knowledge_page_versions_response>(text);
}

std::string
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_response>::write_msgpack(
    const ores::orgmode::messaging::list_knowledge_page_versions_response& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::list_knowledge_page_versions_response
wire_traits<ores::orgmode::messaging::list_knowledge_page_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::list_knowledge_page_versions_response>(
        data);
}

std::string wire_traits<ores::orgmode::messaging::get_knowledge_page_version_request>::write_json(
    const ores::orgmode::messaging::get_knowledge_page_version_request& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::get_knowledge_page_version_request
wire_traits<ores::orgmode::messaging::get_knowledge_page_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::get_knowledge_page_version_request>(text);
}

std::string
wire_traits<ores::orgmode::messaging::get_knowledge_page_version_request>::write_msgpack(
    const ores::orgmode::messaging::get_knowledge_page_version_request& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::get_knowledge_page_version_request
wire_traits<ores::orgmode::messaging::get_knowledge_page_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::get_knowledge_page_version_request>(data);
}

std::string wire_traits<ores::orgmode::messaging::get_knowledge_page_version_response>::write_json(
    const ores::orgmode::messaging::get_knowledge_page_version_response& v) {
    return object::write_json(v);
}

ores::orgmode::messaging::get_knowledge_page_version_response
wire_traits<ores::orgmode::messaging::get_knowledge_page_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::orgmode::messaging::get_knowledge_page_version_response>(text);
}

std::string
wire_traits<ores::orgmode::messaging::get_knowledge_page_version_response>::write_msgpack(
    const ores::orgmode::messaging::get_knowledge_page_version_response& v) {
    return object::write_msgpack(v);
}

ores::orgmode::messaging::get_knowledge_page_version_response
wire_traits<ores::orgmode::messaging::get_knowledge_page_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::orgmode::messaging::get_knowledge_page_version_response>(
        data);
}

}
