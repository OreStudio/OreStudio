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
#include "ores.ore.api/messaging/currency_config_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::ore::messaging::export_currency_config_request>::write_json(
    const ores::ore::messaging::export_currency_config_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::export_currency_config_request
wire_traits<ores::ore::messaging::export_currency_config_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::export_currency_config_request>(text);
}

std::string wire_traits<ores::ore::messaging::export_currency_config_request>::write_msgpack(
    const ores::ore::messaging::export_currency_config_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::export_currency_config_request
wire_traits<ores::ore::messaging::export_currency_config_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::export_currency_config_request>(data);
}

std::string wire_traits<ores::ore::messaging::export_currency_config_response>::write_json(
    const ores::ore::messaging::export_currency_config_response& v) {
    return object::write_json(v);
}

ores::ore::messaging::export_currency_config_response
wire_traits<ores::ore::messaging::export_currency_config_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::export_currency_config_response>(text);
}

std::string wire_traits<ores::ore::messaging::export_currency_config_response>::write_msgpack(
    const ores::ore::messaging::export_currency_config_response& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::export_currency_config_response
wire_traits<ores::ore::messaging::export_currency_config_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::export_currency_config_response>(data);
}

std::string wire_traits<ores::ore::messaging::currency_import_row>::write_json(
    const ores::ore::messaging::currency_import_row& v) {
    return object::write_json(v);
}

ores::ore::messaging::currency_import_row
wire_traits<ores::ore::messaging::currency_import_row>::read_json(std::string_view text) {
    return object::read_json<ores::ore::messaging::currency_import_row>(text);
}

std::string wire_traits<ores::ore::messaging::currency_import_row>::write_msgpack(
    const ores::ore::messaging::currency_import_row& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::currency_import_row
wire_traits<ores::ore::messaging::currency_import_row>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::currency_import_row>(data);
}

std::string wire_traits<ores::ore::messaging::currency_import_issue>::write_json(
    const ores::ore::messaging::currency_import_issue& v) {
    return object::write_json(v);
}

ores::ore::messaging::currency_import_issue
wire_traits<ores::ore::messaging::currency_import_issue>::read_json(std::string_view text) {
    return object::read_json<ores::ore::messaging::currency_import_issue>(text);
}

std::string wire_traits<ores::ore::messaging::currency_import_issue>::write_msgpack(
    const ores::ore::messaging::currency_import_issue& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::currency_import_issue
wire_traits<ores::ore::messaging::currency_import_issue>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::currency_import_issue>(data);
}

std::string wire_traits<ores::ore::messaging::preview_currency_config_import_request>::write_json(
    const ores::ore::messaging::preview_currency_config_import_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::preview_currency_config_import_request
wire_traits<ores::ore::messaging::preview_currency_config_import_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::preview_currency_config_import_request>(text);
}

std::string
wire_traits<ores::ore::messaging::preview_currency_config_import_request>::write_msgpack(
    const ores::ore::messaging::preview_currency_config_import_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::preview_currency_config_import_request
wire_traits<ores::ore::messaging::preview_currency_config_import_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::preview_currency_config_import_request>(data);
}

std::string wire_traits<ores::ore::messaging::preview_currency_config_import_response>::write_json(
    const ores::ore::messaging::preview_currency_config_import_response& v) {
    return object::write_json(v);
}

ores::ore::messaging::preview_currency_config_import_response
wire_traits<ores::ore::messaging::preview_currency_config_import_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::preview_currency_config_import_response>(text);
}

std::string
wire_traits<ores::ore::messaging::preview_currency_config_import_response>::write_msgpack(
    const ores::ore::messaging::preview_currency_config_import_response& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::preview_currency_config_import_response
wire_traits<ores::ore::messaging::preview_currency_config_import_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::preview_currency_config_import_response>(
        data);
}

std::string wire_traits<ores::ore::messaging::currency_import_choice>::write_json(
    const ores::ore::messaging::currency_import_choice& v) {
    return object::write_json(v);
}

ores::ore::messaging::currency_import_choice
wire_traits<ores::ore::messaging::currency_import_choice>::read_json(std::string_view text) {
    return object::read_json<ores::ore::messaging::currency_import_choice>(text);
}

std::string wire_traits<ores::ore::messaging::currency_import_choice>::write_msgpack(
    const ores::ore::messaging::currency_import_choice& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::currency_import_choice
wire_traits<ores::ore::messaging::currency_import_choice>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::currency_import_choice>(data);
}

std::string wire_traits<ores::ore::messaging::import_currency_config_request>::write_json(
    const ores::ore::messaging::import_currency_config_request& v) {
    return object::write_json(v);
}

ores::ore::messaging::import_currency_config_request
wire_traits<ores::ore::messaging::import_currency_config_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::import_currency_config_request>(text);
}

std::string wire_traits<ores::ore::messaging::import_currency_config_request>::write_msgpack(
    const ores::ore::messaging::import_currency_config_request& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::import_currency_config_request
wire_traits<ores::ore::messaging::import_currency_config_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::import_currency_config_request>(data);
}

std::string wire_traits<ores::ore::messaging::currency_import_error>::write_json(
    const ores::ore::messaging::currency_import_error& v) {
    return object::write_json(v);
}

ores::ore::messaging::currency_import_error
wire_traits<ores::ore::messaging::currency_import_error>::read_json(std::string_view text) {
    return object::read_json<ores::ore::messaging::currency_import_error>(text);
}

std::string wire_traits<ores::ore::messaging::currency_import_error>::write_msgpack(
    const ores::ore::messaging::currency_import_error& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::currency_import_error
wire_traits<ores::ore::messaging::currency_import_error>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::currency_import_error>(data);
}

std::string wire_traits<ores::ore::messaging::import_currency_config_response>::write_json(
    const ores::ore::messaging::import_currency_config_response& v) {
    return object::write_json(v);
}

ores::ore::messaging::import_currency_config_response
wire_traits<ores::ore::messaging::import_currency_config_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::ore::messaging::import_currency_config_response>(text);
}

std::string wire_traits<ores::ore::messaging::import_currency_config_response>::write_msgpack(
    const ores::ore::messaging::import_currency_config_response& v) {
    return object::write_msgpack(v);
}

ores::ore::messaging::import_currency_config_response
wire_traits<ores::ore::messaging::import_currency_config_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::ore::messaging::import_currency_config_response>(data);
}

}
