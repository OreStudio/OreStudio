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
#include "ores.refdata.api/domain/counterparty_change_codec.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_change_codec.hpp"
#include "ores.refdata.api/domain/party_counterparty_change_codec.hpp"
#include "ores.refdata.api/messaging/counterparty_proposal_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::counterparty_line_outcome>::write_json(
    const ores::refdata::messaging::counterparty_line_outcome& v) {
    return object::write_json(v);
}

ores::refdata::messaging::counterparty_line_outcome
wire_traits<ores::refdata::messaging::counterparty_line_outcome>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::counterparty_line_outcome>(text);
}

std::string wire_traits<ores::refdata::messaging::counterparty_line_outcome>::write_msgpack(
    const ores::refdata::messaging::counterparty_line_outcome& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::counterparty_line_outcome
wire_traits<ores::refdata::messaging::counterparty_line_outcome>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::counterparty_line_outcome>(data);
}

std::string wire_traits<ores::refdata::messaging::preview_counterparty_changes_request>::write_json(
    const ores::refdata::messaging::preview_counterparty_changes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::preview_counterparty_changes_request
wire_traits<ores::refdata::messaging::preview_counterparty_changes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::preview_counterparty_changes_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::preview_counterparty_changes_request>::write_msgpack(
    const ores::refdata::messaging::preview_counterparty_changes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::preview_counterparty_changes_request
wire_traits<ores::refdata::messaging::preview_counterparty_changes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::preview_counterparty_changes_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::preview_counterparty_changes_response>::write_json(
    const ores::refdata::messaging::preview_counterparty_changes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::preview_counterparty_changes_response
wire_traits<ores::refdata::messaging::preview_counterparty_changes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::preview_counterparty_changes_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::preview_counterparty_changes_response>::write_msgpack(
    const ores::refdata::messaging::preview_counterparty_changes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::preview_counterparty_changes_response
wire_traits<ores::refdata::messaging::preview_counterparty_changes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::preview_counterparty_changes_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::raise_counterparty_changes_request>::write_json(
    const ores::refdata::messaging::raise_counterparty_changes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::raise_counterparty_changes_request
wire_traits<ores::refdata::messaging::raise_counterparty_changes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::raise_counterparty_changes_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::raise_counterparty_changes_request>::write_msgpack(
    const ores::refdata::messaging::raise_counterparty_changes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::raise_counterparty_changes_request
wire_traits<ores::refdata::messaging::raise_counterparty_changes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::raise_counterparty_changes_request>(data);
}

std::string wire_traits<ores::refdata::messaging::raise_counterparty_changes_response>::write_json(
    const ores::refdata::messaging::raise_counterparty_changes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::raise_counterparty_changes_response
wire_traits<ores::refdata::messaging::raise_counterparty_changes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::raise_counterparty_changes_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::raise_counterparty_changes_response>::write_msgpack(
    const ores::refdata::messaging::raise_counterparty_changes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::raise_counterparty_changes_response
wire_traits<ores::refdata::messaging::raise_counterparty_changes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::raise_counterparty_changes_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::counterparty_field_delta>::write_json(
    const ores::refdata::messaging::counterparty_field_delta& v) {
    return object::write_json(v);
}

ores::refdata::messaging::counterparty_field_delta
wire_traits<ores::refdata::messaging::counterparty_field_delta>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::counterparty_field_delta>(text);
}

std::string wire_traits<ores::refdata::messaging::counterparty_field_delta>::write_msgpack(
    const ores::refdata::messaging::counterparty_field_delta& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::counterparty_field_delta
wire_traits<ores::refdata::messaging::counterparty_field_delta>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::counterparty_field_delta>(data);
}

std::string wire_traits<ores::refdata::messaging::counterparty_change_delta>::write_json(
    const ores::refdata::messaging::counterparty_change_delta& v) {
    return object::write_json(v);
}

ores::refdata::messaging::counterparty_change_delta
wire_traits<ores::refdata::messaging::counterparty_change_delta>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::counterparty_change_delta>(text);
}

std::string wire_traits<ores::refdata::messaging::counterparty_change_delta>::write_msgpack(
    const ores::refdata::messaging::counterparty_change_delta& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::counterparty_change_delta
wire_traits<ores::refdata::messaging::counterparty_change_delta>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::counterparty_change_delta>(data);
}

std::string wire_traits<ores::refdata::messaging::get_counterparty_changes_request>::write_json(
    const ores::refdata::messaging::get_counterparty_changes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_counterparty_changes_request
wire_traits<ores::refdata::messaging::get_counterparty_changes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_counterparty_changes_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_counterparty_changes_request>::write_msgpack(
    const ores::refdata::messaging::get_counterparty_changes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_counterparty_changes_request
wire_traits<ores::refdata::messaging::get_counterparty_changes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_counterparty_changes_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_counterparty_changes_response>::write_json(
    const ores::refdata::messaging::get_counterparty_changes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_counterparty_changes_response
wire_traits<ores::refdata::messaging::get_counterparty_changes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_counterparty_changes_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_counterparty_changes_response>::write_msgpack(
    const ores::refdata::messaging::get_counterparty_changes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_counterparty_changes_response
wire_traits<ores::refdata::messaging::get_counterparty_changes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_counterparty_changes_response>(data);
}

std::string wire_traits<ores::refdata::messaging::recheck_counterparty_changes_request>::write_json(
    const ores::refdata::messaging::recheck_counterparty_changes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::recheck_counterparty_changes_request
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::recheck_counterparty_changes_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_request>::write_msgpack(
    const ores::refdata::messaging::recheck_counterparty_changes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::recheck_counterparty_changes_request
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::recheck_counterparty_changes_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_response>::write_json(
    const ores::refdata::messaging::recheck_counterparty_changes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::recheck_counterparty_changes_response
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::recheck_counterparty_changes_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_response>::write_msgpack(
    const ores::refdata::messaging::recheck_counterparty_changes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::recheck_counterparty_changes_response
wire_traits<ores::refdata::messaging::recheck_counterparty_changes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::recheck_counterparty_changes_response>(
        data);
}

}
