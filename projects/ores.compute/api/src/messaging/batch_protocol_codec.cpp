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
#include "ores.compute.api/domain/batch_codec.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::compute::messaging::batch_key>::write_json(
    const ores::compute::messaging::batch_key& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_key
wire_traits<ores::compute::messaging::batch_key>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_key>(text);
}

std::string wire_traits<ores::compute::messaging::batch_key>::write_msgpack(
    const ores::compute::messaging::batch_key& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_key
wire_traits<ores::compute::messaging::batch_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_key>(data);
}

std::string wire_traits<ores::compute::messaging::batch_write>::write_json(
    const ores::compute::messaging::batch_write& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_write
wire_traits<ores::compute::messaging::batch_write>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_write>(text);
}

std::string wire_traits<ores::compute::messaging::batch_write>::write_msgpack(
    const ores::compute::messaging::batch_write& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_write
wire_traits<ores::compute::messaging::batch_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_write>(data);
}

std::string wire_traits<ores::compute::messaging::batch_change>::write_json(
    const ores::compute::messaging::batch_change& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_change
wire_traits<ores::compute::messaging::batch_change>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_change>(text);
}

std::string wire_traits<ores::compute::messaging::batch_change>::write_msgpack(
    const ores::compute::messaging::batch_change& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_change
wire_traits<ores::compute::messaging::batch_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_change>(data);
}

std::string wire_traits<ores::compute::messaging::batch_removal>::write_json(
    const ores::compute::messaging::batch_removal& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_removal
wire_traits<ores::compute::messaging::batch_removal>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_removal>(text);
}

std::string wire_traits<ores::compute::messaging::batch_removal>::write_msgpack(
    const ores::compute::messaging::batch_removal& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_removal
wire_traits<ores::compute::messaging::batch_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_removal>(data);
}

std::string wire_traits<ores::compute::messaging::batch_lookup>::write_json(
    const ores::compute::messaging::batch_lookup& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_lookup
wire_traits<ores::compute::messaging::batch_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_lookup>(text);
}

std::string wire_traits<ores::compute::messaging::batch_lookup>::write_msgpack(
    const ores::compute::messaging::batch_lookup& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_lookup
wire_traits<ores::compute::messaging::batch_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_lookup>(data);
}

std::string wire_traits<ores::compute::messaging::batches_filter>::write_json(
    const ores::compute::messaging::batches_filter& v) {
    return object::write_json(v);
}

ores::compute::messaging::batches_filter
wire_traits<ores::compute::messaging::batches_filter>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batches_filter>(text);
}

std::string wire_traits<ores::compute::messaging::batches_filter>::write_msgpack(
    const ores::compute::messaging::batches_filter& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batches_filter
wire_traits<ores::compute::messaging::batches_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batches_filter>(data);
}

std::string wire_traits<ores::compute::messaging::batch_event>::write_json(
    const ores::compute::messaging::batch_event& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_event
wire_traits<ores::compute::messaging::batch_event>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_event>(text);
}

std::string wire_traits<ores::compute::messaging::batch_event>::write_msgpack(
    const ores::compute::messaging::batch_event& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_event
wire_traits<ores::compute::messaging::batch_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_event>(data);
}

std::string wire_traits<ores::compute::messaging::batch_version_key>::write_json(
    const ores::compute::messaging::batch_version_key& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_version_key
wire_traits<ores::compute::messaging::batch_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_version_key>(text);
}

std::string wire_traits<ores::compute::messaging::batch_version_key>::write_msgpack(
    const ores::compute::messaging::batch_version_key& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_version_key
wire_traits<ores::compute::messaging::batch_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_version_key>(data);
}

std::string wire_traits<ores::compute::messaging::batch_versions_filter>::write_json(
    const ores::compute::messaging::batch_versions_filter& v) {
    return object::write_json(v);
}

ores::compute::messaging::batch_versions_filter
wire_traits<ores::compute::messaging::batch_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::batch_versions_filter>(text);
}

std::string wire_traits<ores::compute::messaging::batch_versions_filter>::write_msgpack(
    const ores::compute::messaging::batch_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::batch_versions_filter
wire_traits<ores::compute::messaging::batch_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::batch_versions_filter>(data);
}

std::string wire_traits<ores::compute::messaging::list_batches_request>::write_json(
    const ores::compute::messaging::list_batches_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_batches_request
wire_traits<ores::compute::messaging::list_batches_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::list_batches_request>(text);
}

std::string wire_traits<ores::compute::messaging::list_batches_request>::write_msgpack(
    const ores::compute::messaging::list_batches_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_batches_request
wire_traits<ores::compute::messaging::list_batches_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_batches_request>(data);
}

std::string wire_traits<ores::compute::messaging::list_batches_response>::write_json(
    const ores::compute::messaging::list_batches_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_batches_response
wire_traits<ores::compute::messaging::list_batches_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::list_batches_response>(text);
}

std::string wire_traits<ores::compute::messaging::list_batches_response>::write_msgpack(
    const ores::compute::messaging::list_batches_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_batches_response
wire_traits<ores::compute::messaging::list_batches_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_batches_response>(data);
}

std::string wire_traits<ores::compute::messaging::get_batch_request>::write_json(
    const ores::compute::messaging::get_batch_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_batch_request
wire_traits<ores::compute::messaging::get_batch_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_batch_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_batch_request>::write_msgpack(
    const ores::compute::messaging::get_batch_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_batch_request
wire_traits<ores::compute::messaging::get_batch_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_batch_request>(data);
}

std::string wire_traits<ores::compute::messaging::get_batch_response>::write_json(
    const ores::compute::messaging::get_batch_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_batch_response
wire_traits<ores::compute::messaging::get_batch_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_batch_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_batch_response>::write_msgpack(
    const ores::compute::messaging::get_batch_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_batch_response
wire_traits<ores::compute::messaging::get_batch_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_batch_response>(data);
}

std::string wire_traits<ores::compute::messaging::get_many_batches_request>::write_json(
    const ores::compute::messaging::get_many_batches_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_many_batches_request
wire_traits<ores::compute::messaging::get_many_batches_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_many_batches_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_many_batches_request>::write_msgpack(
    const ores::compute::messaging::get_many_batches_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_many_batches_request
wire_traits<ores::compute::messaging::get_many_batches_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_many_batches_request>(data);
}

std::string wire_traits<ores::compute::messaging::get_many_batches_response>::write_json(
    const ores::compute::messaging::get_many_batches_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_many_batches_response
wire_traits<ores::compute::messaging::get_many_batches_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_many_batches_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_many_batches_response>::write_msgpack(
    const ores::compute::messaging::get_many_batches_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_many_batches_response
wire_traits<ores::compute::messaging::get_many_batches_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_many_batches_response>(data);
}

std::string wire_traits<ores::compute::messaging::put_batch_request>::write_json(
    const ores::compute::messaging::put_batch_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_batch_request
wire_traits<ores::compute::messaging::put_batch_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::put_batch_request>(text);
}

std::string wire_traits<ores::compute::messaging::put_batch_request>::write_msgpack(
    const ores::compute::messaging::put_batch_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_batch_request
wire_traits<ores::compute::messaging::put_batch_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_batch_request>(data);
}

std::string wire_traits<ores::compute::messaging::put_batch_response>::write_json(
    const ores::compute::messaging::put_batch_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_batch_response
wire_traits<ores::compute::messaging::put_batch_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::put_batch_response>(text);
}

std::string wire_traits<ores::compute::messaging::put_batch_response>::write_msgpack(
    const ores::compute::messaging::put_batch_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_batch_response
wire_traits<ores::compute::messaging::put_batch_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_batch_response>(data);
}

std::string wire_traits<ores::compute::messaging::put_many_batches_request>::write_json(
    const ores::compute::messaging::put_many_batches_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_many_batches_request
wire_traits<ores::compute::messaging::put_many_batches_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::put_many_batches_request>(text);
}

std::string wire_traits<ores::compute::messaging::put_many_batches_request>::write_msgpack(
    const ores::compute::messaging::put_many_batches_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_many_batches_request
wire_traits<ores::compute::messaging::put_many_batches_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_many_batches_request>(data);
}

std::string wire_traits<ores::compute::messaging::put_many_batches_response>::write_json(
    const ores::compute::messaging::put_many_batches_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_many_batches_response
wire_traits<ores::compute::messaging::put_many_batches_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::put_many_batches_response>(text);
}

std::string wire_traits<ores::compute::messaging::put_many_batches_response>::write_msgpack(
    const ores::compute::messaging::put_many_batches_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_many_batches_response
wire_traits<ores::compute::messaging::put_many_batches_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_many_batches_response>(data);
}

std::string wire_traits<ores::compute::messaging::delete_batch_request>::write_json(
    const ores::compute::messaging::delete_batch_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_batch_request
wire_traits<ores::compute::messaging::delete_batch_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_batch_request>(text);
}

std::string wire_traits<ores::compute::messaging::delete_batch_request>::write_msgpack(
    const ores::compute::messaging::delete_batch_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_batch_request
wire_traits<ores::compute::messaging::delete_batch_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::delete_batch_request>(data);
}

std::string wire_traits<ores::compute::messaging::delete_batch_response>::write_json(
    const ores::compute::messaging::delete_batch_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_batch_response
wire_traits<ores::compute::messaging::delete_batch_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_batch_response>(text);
}

std::string wire_traits<ores::compute::messaging::delete_batch_response>::write_msgpack(
    const ores::compute::messaging::delete_batch_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_batch_response
wire_traits<ores::compute::messaging::delete_batch_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::delete_batch_response>(data);
}

std::string wire_traits<ores::compute::messaging::delete_many_batches_request>::write_json(
    const ores::compute::messaging::delete_many_batches_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_many_batches_request
wire_traits<ores::compute::messaging::delete_many_batches_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_many_batches_request>(text);
}

std::string wire_traits<ores::compute::messaging::delete_many_batches_request>::write_msgpack(
    const ores::compute::messaging::delete_many_batches_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_many_batches_request
wire_traits<ores::compute::messaging::delete_many_batches_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::delete_many_batches_request>(data);
}

std::string wire_traits<ores::compute::messaging::delete_many_batches_response>::write_json(
    const ores::compute::messaging::delete_many_batches_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_many_batches_response
wire_traits<ores::compute::messaging::delete_many_batches_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_many_batches_response>(text);
}

std::string wire_traits<ores::compute::messaging::delete_many_batches_response>::write_msgpack(
    const ores::compute::messaging::delete_many_batches_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_many_batches_response
wire_traits<ores::compute::messaging::delete_many_batches_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::delete_many_batches_response>(data);
}

std::string wire_traits<ores::compute::messaging::list_batch_versions_request>::write_json(
    const ores::compute::messaging::list_batch_versions_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_batch_versions_request
wire_traits<ores::compute::messaging::list_batch_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::list_batch_versions_request>(text);
}

std::string wire_traits<ores::compute::messaging::list_batch_versions_request>::write_msgpack(
    const ores::compute::messaging::list_batch_versions_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_batch_versions_request
wire_traits<ores::compute::messaging::list_batch_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_batch_versions_request>(data);
}

std::string wire_traits<ores::compute::messaging::list_batch_versions_response>::write_json(
    const ores::compute::messaging::list_batch_versions_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_batch_versions_response
wire_traits<ores::compute::messaging::list_batch_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::list_batch_versions_response>(text);
}

std::string wire_traits<ores::compute::messaging::list_batch_versions_response>::write_msgpack(
    const ores::compute::messaging::list_batch_versions_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_batch_versions_response
wire_traits<ores::compute::messaging::list_batch_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_batch_versions_response>(data);
}

std::string wire_traits<ores::compute::messaging::get_batch_version_request>::write_json(
    const ores::compute::messaging::get_batch_version_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_batch_version_request
wire_traits<ores::compute::messaging::get_batch_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::get_batch_version_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_batch_version_request>::write_msgpack(
    const ores::compute::messaging::get_batch_version_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_batch_version_request
wire_traits<ores::compute::messaging::get_batch_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_batch_version_request>(data);
}

std::string wire_traits<ores::compute::messaging::get_batch_version_response>::write_json(
    const ores::compute::messaging::get_batch_version_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_batch_version_response
wire_traits<ores::compute::messaging::get_batch_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::get_batch_version_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_batch_version_response>::write_msgpack(
    const ores::compute::messaging::get_batch_version_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_batch_version_response
wire_traits<ores::compute::messaging::get_batch_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_batch_version_response>(data);
}

}
