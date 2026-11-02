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
#include "ores.marketdata.api/domain/supplier_codec.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::supplier_key>::write_json(
    const ores::marketdata::messaging::supplier_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::supplier_key
wire_traits<ores::marketdata::messaging::supplier_key>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::supplier_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::supplier_key>::write_msgpack(
    const ores::marketdata::messaging::supplier_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::supplier_key
wire_traits<ores::marketdata::messaging::supplier_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::supplier_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::supplier_lookup>::write_json(
    const ores::marketdata::messaging::supplier_lookup& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::supplier_lookup
wire_traits<ores::marketdata::messaging::supplier_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::supplier_lookup>(text);
}

std::string wire_traits<ores::marketdata::messaging::supplier_lookup>::write_msgpack(
    const ores::marketdata::messaging::supplier_lookup& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::supplier_lookup
wire_traits<ores::marketdata::messaging::supplier_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::supplier_lookup>(data);
}

std::string wire_traits<ores::marketdata::messaging::suppliers_filter>::write_json(
    const ores::marketdata::messaging::suppliers_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::suppliers_filter
wire_traits<ores::marketdata::messaging::suppliers_filter>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::suppliers_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::suppliers_filter>::write_msgpack(
    const ores::marketdata::messaging::suppliers_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::suppliers_filter
wire_traits<ores::marketdata::messaging::suppliers_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::suppliers_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::supplier_event>::write_json(
    const ores::marketdata::messaging::supplier_event& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::supplier_event
wire_traits<ores::marketdata::messaging::supplier_event>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::supplier_event>(text);
}

std::string wire_traits<ores::marketdata::messaging::supplier_event>::write_msgpack(
    const ores::marketdata::messaging::supplier_event& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::supplier_event
wire_traits<ores::marketdata::messaging::supplier_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::supplier_event>(data);
}

std::string wire_traits<ores::marketdata::messaging::supplier_version_key>::write_json(
    const ores::marketdata::messaging::supplier_version_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::supplier_version_key
wire_traits<ores::marketdata::messaging::supplier_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::supplier_version_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::supplier_version_key>::write_msgpack(
    const ores::marketdata::messaging::supplier_version_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::supplier_version_key
wire_traits<ores::marketdata::messaging::supplier_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::supplier_version_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::supplier_versions_filter>::write_json(
    const ores::marketdata::messaging::supplier_versions_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::supplier_versions_filter
wire_traits<ores::marketdata::messaging::supplier_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::supplier_versions_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::supplier_versions_filter>::write_msgpack(
    const ores::marketdata::messaging::supplier_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::supplier_versions_filter
wire_traits<ores::marketdata::messaging::supplier_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::supplier_versions_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_suppliers_request>::write_json(
    const ores::marketdata::messaging::list_suppliers_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_suppliers_request
wire_traits<ores::marketdata::messaging::list_suppliers_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_suppliers_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_suppliers_request>::write_msgpack(
    const ores::marketdata::messaging::list_suppliers_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_suppliers_request
wire_traits<ores::marketdata::messaging::list_suppliers_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_suppliers_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_suppliers_response>::write_json(
    const ores::marketdata::messaging::list_suppliers_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_suppliers_response
wire_traits<ores::marketdata::messaging::list_suppliers_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_suppliers_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_suppliers_response>::write_msgpack(
    const ores::marketdata::messaging::list_suppliers_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_suppliers_response
wire_traits<ores::marketdata::messaging::list_suppliers_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_suppliers_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_request>::write_json(
    const ores::marketdata::messaging::get_supplier_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_supplier_request
wire_traits<ores::marketdata::messaging::get_supplier_request>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_supplier_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_request>::write_msgpack(
    const ores::marketdata::messaging::get_supplier_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_supplier_request
wire_traits<ores::marketdata::messaging::get_supplier_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_supplier_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_response>::write_json(
    const ores::marketdata::messaging::get_supplier_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_supplier_response
wire_traits<ores::marketdata::messaging::get_supplier_response>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_supplier_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_response>::write_msgpack(
    const ores::marketdata::messaging::get_supplier_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_supplier_response
wire_traits<ores::marketdata::messaging::get_supplier_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_supplier_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_many_suppliers_request>::write_json(
    const ores::marketdata::messaging::get_many_suppliers_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_suppliers_request
wire_traits<ores::marketdata::messaging::get_many_suppliers_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_suppliers_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_many_suppliers_request>::write_msgpack(
    const ores::marketdata::messaging::get_many_suppliers_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_suppliers_request
wire_traits<ores::marketdata::messaging::get_many_suppliers_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_suppliers_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_many_suppliers_response>::write_json(
    const ores::marketdata::messaging::get_many_suppliers_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_suppliers_response
wire_traits<ores::marketdata::messaging::get_many_suppliers_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_suppliers_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_many_suppliers_response>::write_msgpack(
    const ores::marketdata::messaging::get_many_suppliers_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_suppliers_response
wire_traits<ores::marketdata::messaging::get_many_suppliers_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_suppliers_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_supplier_versions_request>::write_json(
    const ores::marketdata::messaging::list_supplier_versions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_supplier_versions_request
wire_traits<ores::marketdata::messaging::list_supplier_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_supplier_versions_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_supplier_versions_request>::write_msgpack(
    const ores::marketdata::messaging::list_supplier_versions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_supplier_versions_request
wire_traits<ores::marketdata::messaging::list_supplier_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_supplier_versions_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_supplier_versions_response>::write_json(
    const ores::marketdata::messaging::list_supplier_versions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_supplier_versions_response
wire_traits<ores::marketdata::messaging::list_supplier_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_supplier_versions_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_supplier_versions_response>::write_msgpack(
    const ores::marketdata::messaging::list_supplier_versions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_supplier_versions_response
wire_traits<ores::marketdata::messaging::list_supplier_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_supplier_versions_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_version_request>::write_json(
    const ores::marketdata::messaging::get_supplier_version_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_supplier_version_request
wire_traits<ores::marketdata::messaging::get_supplier_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_supplier_version_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_version_request>::write_msgpack(
    const ores::marketdata::messaging::get_supplier_version_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_supplier_version_request
wire_traits<ores::marketdata::messaging::get_supplier_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_supplier_version_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_version_response>::write_json(
    const ores::marketdata::messaging::get_supplier_version_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_supplier_version_response
wire_traits<ores::marketdata::messaging::get_supplier_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_supplier_version_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_supplier_version_response>::write_msgpack(
    const ores::marketdata::messaging::get_supplier_version_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_supplier_version_response
wire_traits<ores::marketdata::messaging::get_supplier_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_supplier_version_response>(data);
}

}
