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
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::party_summary>::write_json(
    const ores::iam::messaging::party_summary& v) {
    return object::write_json(v);
}

ores::iam::messaging::party_summary
wire_traits<ores::iam::messaging::party_summary>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::party_summary>(text);
}

std::string wire_traits<ores::iam::messaging::party_summary>::write_msgpack(
    const ores::iam::messaging::party_summary& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::party_summary
wire_traits<ores::iam::messaging::party_summary>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::party_summary>(data);
}

std::string wire_traits<ores::iam::messaging::database_info>::write_json(
    const ores::iam::messaging::database_info& v) {
    return object::write_json(v);
}

ores::iam::messaging::database_info
wire_traits<ores::iam::messaging::database_info>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::database_info>(text);
}

std::string wire_traits<ores::iam::messaging::database_info>::write_msgpack(
    const ores::iam::messaging::database_info& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::database_info
wire_traits<ores::iam::messaging::database_info>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::database_info>(data);
}

std::string wire_traits<ores::iam::messaging::login_request>::write_json(
    const ores::iam::messaging::login_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::login_request
wire_traits<ores::iam::messaging::login_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::login_request>(text);
}

std::string wire_traits<ores::iam::messaging::login_request>::write_msgpack(
    const ores::iam::messaging::login_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::login_request
wire_traits<ores::iam::messaging::login_request>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::login_request>(data);
}

std::string wire_traits<ores::iam::messaging::login_response>::write_json(
    const ores::iam::messaging::login_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::login_response
wire_traits<ores::iam::messaging::login_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::login_response>(text);
}

std::string wire_traits<ores::iam::messaging::login_response>::write_msgpack(
    const ores::iam::messaging::login_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::login_response
wire_traits<ores::iam::messaging::login_response>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::login_response>(data);
}

std::string wire_traits<ores::iam::messaging::logout_request>::write_json(
    const ores::iam::messaging::logout_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::logout_request
wire_traits<ores::iam::messaging::logout_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::logout_request>(text);
}

std::string wire_traits<ores::iam::messaging::logout_request>::write_msgpack(
    const ores::iam::messaging::logout_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::logout_request
wire_traits<ores::iam::messaging::logout_request>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::logout_request>(data);
}

std::string wire_traits<ores::iam::messaging::logout_response>::write_json(
    const ores::iam::messaging::logout_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::logout_response
wire_traits<ores::iam::messaging::logout_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::logout_response>(text);
}

std::string wire_traits<ores::iam::messaging::logout_response>::write_msgpack(
    const ores::iam::messaging::logout_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::logout_response
wire_traits<ores::iam::messaging::logout_response>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::logout_response>(data);
}

std::string wire_traits<ores::iam::messaging::public_key_request>::write_json(
    const ores::iam::messaging::public_key_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::public_key_request
wire_traits<ores::iam::messaging::public_key_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::public_key_request>(text);
}

std::string wire_traits<ores::iam::messaging::public_key_request>::write_msgpack(
    const ores::iam::messaging::public_key_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::public_key_request
wire_traits<ores::iam::messaging::public_key_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::public_key_request>(data);
}

std::string wire_traits<ores::iam::messaging::refresh_request>::write_json(
    const ores::iam::messaging::refresh_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::refresh_request
wire_traits<ores::iam::messaging::refresh_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::refresh_request>(text);
}

std::string wire_traits<ores::iam::messaging::refresh_request>::write_msgpack(
    const ores::iam::messaging::refresh_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::refresh_request
wire_traits<ores::iam::messaging::refresh_request>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::refresh_request>(data);
}

std::string wire_traits<ores::iam::messaging::refresh_response>::write_json(
    const ores::iam::messaging::refresh_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::refresh_response
wire_traits<ores::iam::messaging::refresh_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::refresh_response>(text);
}

std::string wire_traits<ores::iam::messaging::refresh_response>::write_msgpack(
    const ores::iam::messaging::refresh_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::refresh_response
wire_traits<ores::iam::messaging::refresh_response>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::refresh_response>(data);
}

std::string wire_traits<ores::iam::messaging::service_login_request>::write_json(
    const ores::iam::messaging::service_login_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::service_login_request
wire_traits<ores::iam::messaging::service_login_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::service_login_request>(text);
}

std::string wire_traits<ores::iam::messaging::service_login_request>::write_msgpack(
    const ores::iam::messaging::service_login_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::service_login_request
wire_traits<ores::iam::messaging::service_login_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::service_login_request>(data);
}

std::string wire_traits<ores::iam::messaging::service_login_response>::write_json(
    const ores::iam::messaging::service_login_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::service_login_response
wire_traits<ores::iam::messaging::service_login_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::service_login_response>(text);
}

std::string wire_traits<ores::iam::messaging::service_login_response>::write_msgpack(
    const ores::iam::messaging::service_login_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::service_login_response
wire_traits<ores::iam::messaging::service_login_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::service_login_response>(data);
}

}
