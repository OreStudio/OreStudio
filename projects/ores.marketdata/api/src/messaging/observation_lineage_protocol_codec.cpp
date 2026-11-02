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
#include "ores.marketdata.api/domain/observation_lineage_codec.hpp"
#include "ores.marketdata.api/messaging/observation_lineage_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::observation_lineage_key>::write_json(
    const ores::marketdata::messaging::observation_lineage_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_key
wire_traits<ores::marketdata::messaging::observation_lineage_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_key>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_key
wire_traits<ores::marketdata::messaging::observation_lineage_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_write>::write_json(
    const ores::marketdata::messaging::observation_lineage_write& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_write
wire_traits<ores::marketdata::messaging::observation_lineage_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_write>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_write>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_write& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_write
wire_traits<ores::marketdata::messaging::observation_lineage_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_write>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_change>::write_json(
    const ores::marketdata::messaging::observation_lineage_change& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_change
wire_traits<ores::marketdata::messaging::observation_lineage_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_change>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_change>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_change& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_change
wire_traits<ores::marketdata::messaging::observation_lineage_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_change>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_removal>::write_json(
    const ores::marketdata::messaging::observation_lineage_removal& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_removal
wire_traits<ores::marketdata::messaging::observation_lineage_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_removal>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_removal>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_removal& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_removal
wire_traits<ores::marketdata::messaging::observation_lineage_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_removal>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_lookup>::write_json(
    const ores::marketdata::messaging::observation_lineage_lookup& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_lookup
wire_traits<ores::marketdata::messaging::observation_lineage_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_lookup>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_lookup>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_lookup& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_lookup
wire_traits<ores::marketdata::messaging::observation_lineage_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_lookup>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineages_filter>::write_json(
    const ores::marketdata::messaging::observation_lineages_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineages_filter
wire_traits<ores::marketdata::messaging::observation_lineages_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineages_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineages_filter>::write_msgpack(
    const ores::marketdata::messaging::observation_lineages_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineages_filter
wire_traits<ores::marketdata::messaging::observation_lineages_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineages_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_event>::write_json(
    const ores::marketdata::messaging::observation_lineage_event& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_event
wire_traits<ores::marketdata::messaging::observation_lineage_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_event>(text);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_event>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_event& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_event
wire_traits<ores::marketdata::messaging::observation_lineage_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_event>(data);
}

std::string wire_traits<ores::marketdata::messaging::observation_lineage_version_key>::write_json(
    const ores::marketdata::messaging::observation_lineage_version_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_version_key
wire_traits<ores::marketdata::messaging::observation_lineage_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_version_key>(text);
}

std::string
wire_traits<ores::marketdata::messaging::observation_lineage_version_key>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_version_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_version_key
wire_traits<ores::marketdata::messaging::observation_lineage_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_version_key>(data);
}

std::string
wire_traits<ores::marketdata::messaging::observation_lineage_versions_filter>::write_json(
    const ores::marketdata::messaging::observation_lineage_versions_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::observation_lineage_versions_filter
wire_traits<ores::marketdata::messaging::observation_lineage_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::observation_lineage_versions_filter>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::observation_lineage_versions_filter>::write_msgpack(
    const ores::marketdata::messaging::observation_lineage_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::observation_lineage_versions_filter
wire_traits<ores::marketdata::messaging::observation_lineage_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::observation_lineage_versions_filter>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::list_observation_lineages_request>::write_json(
    const ores::marketdata::messaging::list_observation_lineages_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_observation_lineages_request
wire_traits<ores::marketdata::messaging::list_observation_lineages_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_observation_lineages_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineages_request>::write_msgpack(
    const ores::marketdata::messaging::list_observation_lineages_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_observation_lineages_request
wire_traits<ores::marketdata::messaging::list_observation_lineages_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_observation_lineages_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineages_response>::write_json(
    const ores::marketdata::messaging::list_observation_lineages_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_observation_lineages_response
wire_traits<ores::marketdata::messaging::list_observation_lineages_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_observation_lineages_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineages_response>::write_msgpack(
    const ores::marketdata::messaging::list_observation_lineages_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_observation_lineages_response
wire_traits<ores::marketdata::messaging::list_observation_lineages_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_observation_lineages_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::get_observation_lineage_request>::write_json(
    const ores::marketdata::messaging::get_observation_lineage_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_observation_lineage_request
wire_traits<ores::marketdata::messaging::get_observation_lineage_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_observation_lineage_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_observation_lineage_request>::write_msgpack(
    const ores::marketdata::messaging::get_observation_lineage_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_observation_lineage_request
wire_traits<ores::marketdata::messaging::get_observation_lineage_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_observation_lineage_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_observation_lineage_response>::write_json(
    const ores::marketdata::messaging::get_observation_lineage_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_observation_lineage_response
wire_traits<ores::marketdata::messaging::get_observation_lineage_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_observation_lineage_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_observation_lineage_response>::write_msgpack(
    const ores::marketdata::messaging::get_observation_lineage_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_observation_lineage_response
wire_traits<ores::marketdata::messaging::get_observation_lineage_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_observation_lineage_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_request>::write_json(
    const ores::marketdata::messaging::get_many_observation_lineages_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_observation_lineages_request
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_observation_lineages_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_request>::write_msgpack(
    const ores::marketdata::messaging::get_many_observation_lineages_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_observation_lineages_request
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_observation_lineages_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_response>::write_json(
    const ores::marketdata::messaging::get_many_observation_lineages_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_observation_lineages_response
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_observation_lineages_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_response>::write_msgpack(
    const ores::marketdata::messaging::get_many_observation_lineages_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_observation_lineages_response
wire_traits<ores::marketdata::messaging::get_many_observation_lineages_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::get_many_observation_lineages_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_observation_lineage_request>::write_json(
    const ores::marketdata::messaging::put_observation_lineage_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_observation_lineage_request
wire_traits<ores::marketdata::messaging::put_observation_lineage_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_observation_lineage_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::put_observation_lineage_request>::write_msgpack(
    const ores::marketdata::messaging::put_observation_lineage_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_observation_lineage_request
wire_traits<ores::marketdata::messaging::put_observation_lineage_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_observation_lineage_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_observation_lineage_response>::write_json(
    const ores::marketdata::messaging::put_observation_lineage_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_observation_lineage_response
wire_traits<ores::marketdata::messaging::put_observation_lineage_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_observation_lineage_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::put_observation_lineage_response>::write_msgpack(
    const ores::marketdata::messaging::put_observation_lineage_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_observation_lineage_response
wire_traits<ores::marketdata::messaging::put_observation_lineage_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_observation_lineage_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_request>::write_json(
    const ores::marketdata::messaging::put_many_observation_lineages_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_observation_lineages_request
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_observation_lineages_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_request>::write_msgpack(
    const ores::marketdata::messaging::put_many_observation_lineages_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_observation_lineages_request
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_observation_lineages_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_response>::write_json(
    const ores::marketdata::messaging::put_many_observation_lineages_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_observation_lineages_response
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_observation_lineages_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_response>::write_msgpack(
    const ores::marketdata::messaging::put_many_observation_lineages_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_observation_lineages_response
wire_traits<ores::marketdata::messaging::put_many_observation_lineages_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::put_many_observation_lineages_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_observation_lineage_request>::write_json(
    const ores::marketdata::messaging::delete_observation_lineage_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_observation_lineage_request
wire_traits<ores::marketdata::messaging::delete_observation_lineage_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_observation_lineage_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_observation_lineage_request>::write_msgpack(
    const ores::marketdata::messaging::delete_observation_lineage_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_observation_lineage_request
wire_traits<ores::marketdata::messaging::delete_observation_lineage_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_observation_lineage_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_observation_lineage_response>::write_json(
    const ores::marketdata::messaging::delete_observation_lineage_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_observation_lineage_response
wire_traits<ores::marketdata::messaging::delete_observation_lineage_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_observation_lineage_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_observation_lineage_response>::write_msgpack(
    const ores::marketdata::messaging::delete_observation_lineage_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_observation_lineage_response
wire_traits<ores::marketdata::messaging::delete_observation_lineage_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_observation_lineage_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_request>::write_json(
    const ores::marketdata::messaging::delete_many_observation_lineages_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_observation_lineages_request
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_observation_lineages_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_request>::write_msgpack(
    const ores::marketdata::messaging::delete_many_observation_lineages_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_observation_lineages_request
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::delete_many_observation_lineages_request>(data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_response>::write_json(
    const ores::marketdata::messaging::delete_many_observation_lineages_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_observation_lineages_response
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::delete_many_observation_lineages_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_response>::write_msgpack(
    const ores::marketdata::messaging::delete_many_observation_lineages_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_observation_lineages_response
wire_traits<ores::marketdata::messaging::delete_many_observation_lineages_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::delete_many_observation_lineages_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_request>::write_json(
    const ores::marketdata::messaging::list_observation_lineage_versions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_observation_lineage_versions_request
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::list_observation_lineage_versions_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_request>::write_msgpack(
    const ores::marketdata::messaging::list_observation_lineage_versions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_observation_lineage_versions_request
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::list_observation_lineage_versions_request>(data);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_response>::write_json(
    const ores::marketdata::messaging::list_observation_lineage_versions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_observation_lineage_versions_response
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::marketdata::messaging::list_observation_lineage_versions_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_response>::write_msgpack(
    const ores::marketdata::messaging::list_observation_lineage_versions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_observation_lineage_versions_response
wire_traits<ores::marketdata::messaging::list_observation_lineage_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::list_observation_lineage_versions_response>(data);
}

std::string
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_request>::write_json(
    const ores::marketdata::messaging::get_observation_lineage_version_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_observation_lineage_version_request
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_observation_lineage_version_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_request>::write_msgpack(
    const ores::marketdata::messaging::get_observation_lineage_version_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_observation_lineage_version_request
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::get_observation_lineage_version_request>(data);
}

std::string
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_response>::write_json(
    const ores::marketdata::messaging::get_observation_lineage_version_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_observation_lineage_version_response
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_observation_lineage_version_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_response>::write_msgpack(
    const ores::marketdata::messaging::get_observation_lineage_version_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_observation_lineage_version_response
wire_traits<ores::marketdata::messaging::get_observation_lineage_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::marketdata::messaging::get_observation_lineage_version_response>(data);
}

}
