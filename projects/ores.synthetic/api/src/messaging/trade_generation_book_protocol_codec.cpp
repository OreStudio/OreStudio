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
#include "ores.synthetic.api/domain/trade_generation_book_codec.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_key>::write_json(
    const ores::synthetic::messaging::trade_generation_book_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_key
wire_traits<ores::synthetic::messaging::trade_generation_book_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_key>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_key
wire_traits<ores::synthetic::messaging::trade_generation_book_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_write>::write_json(
    const ores::synthetic::messaging::trade_generation_book_write& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_write
wire_traits<ores::synthetic::messaging::trade_generation_book_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_write>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_write>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_write& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_write
wire_traits<ores::synthetic::messaging::trade_generation_book_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_write>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_change>::write_json(
    const ores::synthetic::messaging::trade_generation_book_change& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_change
wire_traits<ores::synthetic::messaging::trade_generation_book_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_change>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_change>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_change& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_change
wire_traits<ores::synthetic::messaging::trade_generation_book_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_change>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_removal>::write_json(
    const ores::synthetic::messaging::trade_generation_book_removal& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_removal
wire_traits<ores::synthetic::messaging::trade_generation_book_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_removal>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_removal>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_removal& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_removal
wire_traits<ores::synthetic::messaging::trade_generation_book_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_removal>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_lookup>::write_json(
    const ores::synthetic::messaging::trade_generation_book_lookup& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_lookup
wire_traits<ores::synthetic::messaging::trade_generation_book_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_lookup>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_lookup>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_lookup& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_lookup
wire_traits<ores::synthetic::messaging::trade_generation_book_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_lookup>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_books_filter>::write_json(
    const ores::synthetic::messaging::trade_generation_books_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_books_filter
wire_traits<ores::synthetic::messaging::trade_generation_books_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_books_filter>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_books_filter>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_books_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_books_filter
wire_traits<ores::synthetic::messaging::trade_generation_books_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_books_filter>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_event>::write_json(
    const ores::synthetic::messaging::trade_generation_book_event& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_event
wire_traits<ores::synthetic::messaging::trade_generation_book_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_event>(text);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_event>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_event& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_event
wire_traits<ores::synthetic::messaging::trade_generation_book_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_event>(data);
}

std::string wire_traits<ores::synthetic::messaging::trade_generation_book_version_key>::write_json(
    const ores::synthetic::messaging::trade_generation_book_version_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_version_key
wire_traits<ores::synthetic::messaging::trade_generation_book_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_version_key>(text);
}

std::string
wire_traits<ores::synthetic::messaging::trade_generation_book_version_key>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_version_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_version_key
wire_traits<ores::synthetic::messaging::trade_generation_book_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_version_key>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::trade_generation_book_versions_filter>::write_json(
    const ores::synthetic::messaging::trade_generation_book_versions_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::trade_generation_book_versions_filter
wire_traits<ores::synthetic::messaging::trade_generation_book_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::trade_generation_book_versions_filter>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::trade_generation_book_versions_filter>::write_msgpack(
    const ores::synthetic::messaging::trade_generation_book_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::trade_generation_book_versions_filter
wire_traits<ores::synthetic::messaging::trade_generation_book_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::trade_generation_book_versions_filter>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_books_request>::write_json(
    const ores::synthetic::messaging::list_trade_generation_books_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_trade_generation_books_request
wire_traits<ores::synthetic::messaging::list_trade_generation_books_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_trade_generation_books_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_books_request>::write_msgpack(
    const ores::synthetic::messaging::list_trade_generation_books_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_trade_generation_books_request
wire_traits<ores::synthetic::messaging::list_trade_generation_books_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_trade_generation_books_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_books_response>::write_json(
    const ores::synthetic::messaging::list_trade_generation_books_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_trade_generation_books_response
wire_traits<ores::synthetic::messaging::list_trade_generation_books_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_trade_generation_books_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_books_response>::write_msgpack(
    const ores::synthetic::messaging::list_trade_generation_books_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_trade_generation_books_response
wire_traits<ores::synthetic::messaging::list_trade_generation_books_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_trade_generation_books_response>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::get_trade_generation_book_request>::write_json(
    const ores::synthetic::messaging::get_trade_generation_book_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_trade_generation_book_request
wire_traits<ores::synthetic::messaging::get_trade_generation_book_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_trade_generation_book_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_trade_generation_book_request>::write_msgpack(
    const ores::synthetic::messaging::get_trade_generation_book_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_trade_generation_book_request
wire_traits<ores::synthetic::messaging::get_trade_generation_book_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_trade_generation_book_request>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::get_trade_generation_book_response>::write_json(
    const ores::synthetic::messaging::get_trade_generation_book_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_trade_generation_book_response
wire_traits<ores::synthetic::messaging::get_trade_generation_book_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_trade_generation_book_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_trade_generation_book_response>::write_msgpack(
    const ores::synthetic::messaging::get_trade_generation_book_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_trade_generation_book_response
wire_traits<ores::synthetic::messaging::get_trade_generation_book_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_trade_generation_book_response>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_request>::write_json(
    const ores::synthetic::messaging::get_many_trade_generation_books_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_trade_generation_books_request
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_trade_generation_books_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_request>::write_msgpack(
    const ores::synthetic::messaging::get_many_trade_generation_books_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_trade_generation_books_request
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_many_trade_generation_books_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_response>::write_json(
    const ores::synthetic::messaging::get_many_trade_generation_books_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_trade_generation_books_response
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_trade_generation_books_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_response>::write_msgpack(
    const ores::synthetic::messaging::get_many_trade_generation_books_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_trade_generation_books_response
wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_many_trade_generation_books_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_trade_generation_book_request>::write_json(
    const ores::synthetic::messaging::put_trade_generation_book_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_trade_generation_book_request
wire_traits<ores::synthetic::messaging::put_trade_generation_book_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_trade_generation_book_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::put_trade_generation_book_request>::write_msgpack(
    const ores::synthetic::messaging::put_trade_generation_book_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_trade_generation_book_request
wire_traits<ores::synthetic::messaging::put_trade_generation_book_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_trade_generation_book_request>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::put_trade_generation_book_response>::write_json(
    const ores::synthetic::messaging::put_trade_generation_book_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_trade_generation_book_response
wire_traits<ores::synthetic::messaging::put_trade_generation_book_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_trade_generation_book_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::put_trade_generation_book_response>::write_msgpack(
    const ores::synthetic::messaging::put_trade_generation_book_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_trade_generation_book_response
wire_traits<ores::synthetic::messaging::put_trade_generation_book_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_trade_generation_book_response>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_request>::write_json(
    const ores::synthetic::messaging::put_many_trade_generation_books_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_trade_generation_books_request
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_trade_generation_books_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_request>::write_msgpack(
    const ores::synthetic::messaging::put_many_trade_generation_books_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_trade_generation_books_request
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::put_many_trade_generation_books_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_response>::write_json(
    const ores::synthetic::messaging::put_many_trade_generation_books_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_trade_generation_books_response
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_trade_generation_books_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_response>::write_msgpack(
    const ores::synthetic::messaging::put_many_trade_generation_books_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_trade_generation_books_response
wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::put_many_trade_generation_books_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_request>::write_json(
    const ores::synthetic::messaging::delete_trade_generation_book_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_trade_generation_book_request
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_trade_generation_book_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_request>::write_msgpack(
    const ores::synthetic::messaging::delete_trade_generation_book_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_trade_generation_book_request
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_trade_generation_book_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_response>::write_json(
    const ores::synthetic::messaging::delete_trade_generation_book_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_trade_generation_book_response
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_trade_generation_book_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_response>::write_msgpack(
    const ores::synthetic::messaging::delete_trade_generation_book_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_trade_generation_book_response
wire_traits<ores::synthetic::messaging::delete_trade_generation_book_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_trade_generation_book_response>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_request>::write_json(
    const ores::synthetic::messaging::delete_many_trade_generation_books_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_trade_generation_books_request
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::delete_many_trade_generation_books_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_request>::write_msgpack(
    const ores::synthetic::messaging::delete_many_trade_generation_books_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_trade_generation_books_request
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_trade_generation_books_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_response>::write_json(
    const ores::synthetic::messaging::delete_many_trade_generation_books_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_trade_generation_books_response
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::delete_many_trade_generation_books_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_response>::write_msgpack(
    const ores::synthetic::messaging::delete_many_trade_generation_books_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_trade_generation_books_response
wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_trade_generation_books_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_request>::write_json(
    const ores::synthetic::messaging::list_trade_generation_book_versions_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_trade_generation_book_versions_request
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::list_trade_generation_book_versions_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_request>::write_msgpack(
    const ores::synthetic::messaging::list_trade_generation_book_versions_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_trade_generation_book_versions_request
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_trade_generation_book_versions_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_response>::write_json(
    const ores::synthetic::messaging::list_trade_generation_book_versions_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_trade_generation_book_versions_response
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::list_trade_generation_book_versions_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_response>::
    write_msgpack(
        const ores::synthetic::messaging::list_trade_generation_book_versions_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_trade_generation_book_versions_response
wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_trade_generation_book_versions_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_request>::write_json(
    const ores::synthetic::messaging::get_trade_generation_book_version_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_trade_generation_book_version_request
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_trade_generation_book_version_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_request>::write_msgpack(
    const ores::synthetic::messaging::get_trade_generation_book_version_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_trade_generation_book_version_request
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_trade_generation_book_version_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_response>::write_json(
    const ores::synthetic::messaging::get_trade_generation_book_version_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_trade_generation_book_version_response
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::get_trade_generation_book_version_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_response>::write_msgpack(
    const ores::synthetic::messaging::get_trade_generation_book_version_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_trade_generation_book_version_response
wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_trade_generation_book_version_response>(data);
}

}
