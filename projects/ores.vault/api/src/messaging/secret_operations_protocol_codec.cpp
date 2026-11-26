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
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::vault::messaging::write_secret_request>::write_json(
    const ores::vault::messaging::write_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::write_secret_request
wire_traits<ores::vault::messaging::write_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::write_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::write_secret_request>::write_msgpack(
    const ores::vault::messaging::write_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::write_secret_request
wire_traits<ores::vault::messaging::write_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::write_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::write_secret_response>::write_json(
    const ores::vault::messaging::write_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::write_secret_response
wire_traits<ores::vault::messaging::write_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::write_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::write_secret_response>::write_msgpack(
    const ores::vault::messaging::write_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::write_secret_response
wire_traits<ores::vault::messaging::write_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::write_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::describe_secret_request>::write_json(
    const ores::vault::messaging::describe_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::describe_secret_request
wire_traits<ores::vault::messaging::describe_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::describe_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::describe_secret_request>::write_msgpack(
    const ores::vault::messaging::describe_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::describe_secret_request
wire_traits<ores::vault::messaging::describe_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::describe_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::describe_secret_response>::write_json(
    const ores::vault::messaging::describe_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::describe_secret_response
wire_traits<ores::vault::messaging::describe_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::describe_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::describe_secret_response>::write_msgpack(
    const ores::vault::messaging::describe_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::describe_secret_response
wire_traits<ores::vault::messaging::describe_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::describe_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::resolve_secret_request>::write_json(
    const ores::vault::messaging::resolve_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::resolve_secret_request
wire_traits<ores::vault::messaging::resolve_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::resolve_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::resolve_secret_request>::write_msgpack(
    const ores::vault::messaging::resolve_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::resolve_secret_request
wire_traits<ores::vault::messaging::resolve_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::resolve_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::resolve_secret_response>::write_json(
    const ores::vault::messaging::resolve_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::resolve_secret_response
wire_traits<ores::vault::messaging::resolve_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::resolve_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::resolve_secret_response>::write_msgpack(
    const ores::vault::messaging::resolve_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::resolve_secret_response
wire_traits<ores::vault::messaging::resolve_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::resolve_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::remove_secret_request>::write_json(
    const ores::vault::messaging::remove_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::remove_secret_request
wire_traits<ores::vault::messaging::remove_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::remove_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::remove_secret_request>::write_msgpack(
    const ores::vault::messaging::remove_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::remove_secret_request
wire_traits<ores::vault::messaging::remove_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::remove_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::remove_secret_response>::write_json(
    const ores::vault::messaging::remove_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::remove_secret_response
wire_traits<ores::vault::messaging::remove_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::remove_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::remove_secret_response>::write_msgpack(
    const ores::vault::messaging::remove_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::remove_secret_response
wire_traits<ores::vault::messaging::remove_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::remove_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::rotate_secrets_request>::write_json(
    const ores::vault::messaging::rotate_secrets_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::rotate_secrets_request
wire_traits<ores::vault::messaging::rotate_secrets_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::rotate_secrets_request>(text);
}

std::string wire_traits<ores::vault::messaging::rotate_secrets_request>::write_msgpack(
    const ores::vault::messaging::rotate_secrets_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::rotate_secrets_request
wire_traits<ores::vault::messaging::rotate_secrets_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::rotate_secrets_request>(data);
}

std::string wire_traits<ores::vault::messaging::rotate_secrets_response>::write_json(
    const ores::vault::messaging::rotate_secrets_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::rotate_secrets_response
wire_traits<ores::vault::messaging::rotate_secrets_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::rotate_secrets_response>(text);
}

std::string wire_traits<ores::vault::messaging::rotate_secrets_response>::write_msgpack(
    const ores::vault::messaging::rotate_secrets_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::rotate_secrets_response
wire_traits<ores::vault::messaging::rotate_secrets_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::rotate_secrets_response>(data);
}

std::string wire_traits<ores::vault::messaging::vault_public_key_request>::write_json(
    const ores::vault::messaging::vault_public_key_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::vault_public_key_request
wire_traits<ores::vault::messaging::vault_public_key_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::vault_public_key_request>(text);
}

std::string wire_traits<ores::vault::messaging::vault_public_key_request>::write_msgpack(
    const ores::vault::messaging::vault_public_key_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::vault_public_key_request
wire_traits<ores::vault::messaging::vault_public_key_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::vault_public_key_request>(data);
}

std::string wire_traits<ores::vault::messaging::vault_public_key_response>::write_json(
    const ores::vault::messaging::vault_public_key_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::vault_public_key_response
wire_traits<ores::vault::messaging::vault_public_key_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::vault_public_key_response>(text);
}

std::string wire_traits<ores::vault::messaging::vault_public_key_response>::write_msgpack(
    const ores::vault::messaging::vault_public_key_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::vault_public_key_response
wire_traits<ores::vault::messaging::vault_public_key_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::vault_public_key_response>(data);
}

}
