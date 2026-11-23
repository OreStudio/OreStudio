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
#include "ores.iam.api/domain/agent_call_codec.hpp"
#include "ores.iam.api/domain/agent_grant_codec.hpp"
#include "ores.iam.api/domain/agent_held_write_codec.hpp"
#include "ores.iam.api/domain/agent_key_codec.hpp"
#include "ores.iam.api/messaging/agent_grant_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::create_agent_grant_request>::write_json(
    const ores::iam::messaging::create_agent_grant_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::create_agent_grant_request
wire_traits<ores::iam::messaging::create_agent_grant_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::create_agent_grant_request>(text);
}

std::string wire_traits<ores::iam::messaging::create_agent_grant_request>::write_msgpack(
    const ores::iam::messaging::create_agent_grant_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::create_agent_grant_request
wire_traits<ores::iam::messaging::create_agent_grant_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::create_agent_grant_request>(data);
}

std::string wire_traits<ores::iam::messaging::create_agent_grant_response>::write_json(
    const ores::iam::messaging::create_agent_grant_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::create_agent_grant_response
wire_traits<ores::iam::messaging::create_agent_grant_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::create_agent_grant_response>(text);
}

std::string wire_traits<ores::iam::messaging::create_agent_grant_response>::write_msgpack(
    const ores::iam::messaging::create_agent_grant_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::create_agent_grant_response
wire_traits<ores::iam::messaging::create_agent_grant_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::create_agent_grant_response>(data);
}

std::string wire_traits<ores::iam::messaging::create_agent_key_request>::write_json(
    const ores::iam::messaging::create_agent_key_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::create_agent_key_request
wire_traits<ores::iam::messaging::create_agent_key_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::create_agent_key_request>(text);
}

std::string wire_traits<ores::iam::messaging::create_agent_key_request>::write_msgpack(
    const ores::iam::messaging::create_agent_key_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::create_agent_key_request
wire_traits<ores::iam::messaging::create_agent_key_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::create_agent_key_request>(data);
}

std::string wire_traits<ores::iam::messaging::create_agent_key_response>::write_json(
    const ores::iam::messaging::create_agent_key_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::create_agent_key_response
wire_traits<ores::iam::messaging::create_agent_key_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::create_agent_key_response>(text);
}

std::string wire_traits<ores::iam::messaging::create_agent_key_response>::write_msgpack(
    const ores::iam::messaging::create_agent_key_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::create_agent_key_response
wire_traits<ores::iam::messaging::create_agent_key_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::create_agent_key_response>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_grant_request>::write_json(
    const ores::iam::messaging::revoke_agent_grant_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_agent_grant_request
wire_traits<ores::iam::messaging::revoke_agent_grant_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_agent_grant_request>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_grant_request>::write_msgpack(
    const ores::iam::messaging::revoke_agent_grant_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_agent_grant_request
wire_traits<ores::iam::messaging::revoke_agent_grant_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_agent_grant_request>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_grant_response>::write_json(
    const ores::iam::messaging::revoke_agent_grant_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_agent_grant_response
wire_traits<ores::iam::messaging::revoke_agent_grant_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_agent_grant_response>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_grant_response>::write_msgpack(
    const ores::iam::messaging::revoke_agent_grant_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_agent_grant_response
wire_traits<ores::iam::messaging::revoke_agent_grant_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_agent_grant_response>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_key_request>::write_json(
    const ores::iam::messaging::revoke_agent_key_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_agent_key_request
wire_traits<ores::iam::messaging::revoke_agent_key_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_agent_key_request>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_key_request>::write_msgpack(
    const ores::iam::messaging::revoke_agent_key_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_agent_key_request
wire_traits<ores::iam::messaging::revoke_agent_key_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_agent_key_request>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_key_response>::write_json(
    const ores::iam::messaging::revoke_agent_key_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_agent_key_response
wire_traits<ores::iam::messaging::revoke_agent_key_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_agent_key_response>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_agent_key_response>::write_msgpack(
    const ores::iam::messaging::revoke_agent_key_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_agent_key_response
wire_traits<ores::iam::messaging::revoke_agent_key_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_agent_key_response>(data);
}

std::string wire_traits<ores::iam::messaging::exchange_agent_key_request>::write_json(
    const ores::iam::messaging::exchange_agent_key_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::exchange_agent_key_request
wire_traits<ores::iam::messaging::exchange_agent_key_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::exchange_agent_key_request>(text);
}

std::string wire_traits<ores::iam::messaging::exchange_agent_key_request>::write_msgpack(
    const ores::iam::messaging::exchange_agent_key_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::exchange_agent_key_request
wire_traits<ores::iam::messaging::exchange_agent_key_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::exchange_agent_key_request>(data);
}

std::string wire_traits<ores::iam::messaging::exchange_agent_key_response>::write_json(
    const ores::iam::messaging::exchange_agent_key_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::exchange_agent_key_response
wire_traits<ores::iam::messaging::exchange_agent_key_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::exchange_agent_key_response>(text);
}

std::string wire_traits<ores::iam::messaging::exchange_agent_key_response>::write_msgpack(
    const ores::iam::messaging::exchange_agent_key_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::exchange_agent_key_response
wire_traits<ores::iam::messaging::exchange_agent_key_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::exchange_agent_key_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_grants_request>::write_json(
    const ores::iam::messaging::list_own_agent_grants_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_own_agent_grants_request
wire_traits<ores::iam::messaging::list_own_agent_grants_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_own_agent_grants_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_grants_request>::write_msgpack(
    const ores::iam::messaging::list_own_agent_grants_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_own_agent_grants_request
wire_traits<ores::iam::messaging::list_own_agent_grants_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_own_agent_grants_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_grants_response>::write_json(
    const ores::iam::messaging::list_own_agent_grants_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_own_agent_grants_response
wire_traits<ores::iam::messaging::list_own_agent_grants_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_own_agent_grants_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_grants_response>::write_msgpack(
    const ores::iam::messaging::list_own_agent_grants_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_own_agent_grants_response
wire_traits<ores::iam::messaging::list_own_agent_grants_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_own_agent_grants_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_every_agent_grant_request>::write_json(
    const ores::iam::messaging::list_every_agent_grant_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_every_agent_grant_request
wire_traits<ores::iam::messaging::list_every_agent_grant_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_every_agent_grant_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_every_agent_grant_request>::write_msgpack(
    const ores::iam::messaging::list_every_agent_grant_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_every_agent_grant_request
wire_traits<ores::iam::messaging::list_every_agent_grant_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_every_agent_grant_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_every_agent_grant_response>::write_json(
    const ores::iam::messaging::list_every_agent_grant_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_every_agent_grant_response
wire_traits<ores::iam::messaging::list_every_agent_grant_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_every_agent_grant_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_every_agent_grant_response>::write_msgpack(
    const ores::iam::messaging::list_every_agent_grant_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_every_agent_grant_response
wire_traits<ores::iam::messaging::list_every_agent_grant_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_every_agent_grant_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_activity_request>::write_json(
    const ores::iam::messaging::list_own_agent_activity_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_own_agent_activity_request
wire_traits<ores::iam::messaging::list_own_agent_activity_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_own_agent_activity_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_activity_request>::write_msgpack(
    const ores::iam::messaging::list_own_agent_activity_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_own_agent_activity_request
wire_traits<ores::iam::messaging::list_own_agent_activity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_own_agent_activity_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_activity_response>::write_json(
    const ores::iam::messaging::list_own_agent_activity_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_own_agent_activity_response
wire_traits<ores::iam::messaging::list_own_agent_activity_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_own_agent_activity_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_own_agent_activity_response>::write_msgpack(
    const ores::iam::messaging::list_own_agent_activity_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_own_agent_activity_response
wire_traits<ores::iam::messaging::list_own_agent_activity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_own_agent_activity_response>(data);
}

std::string wire_traits<ores::iam::messaging::issue_agent_replay_token_request>::write_json(
    const ores::iam::messaging::issue_agent_replay_token_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::issue_agent_replay_token_request
wire_traits<ores::iam::messaging::issue_agent_replay_token_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::issue_agent_replay_token_request>(text);
}

std::string wire_traits<ores::iam::messaging::issue_agent_replay_token_request>::write_msgpack(
    const ores::iam::messaging::issue_agent_replay_token_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::issue_agent_replay_token_request
wire_traits<ores::iam::messaging::issue_agent_replay_token_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::issue_agent_replay_token_request>(data);
}

std::string wire_traits<ores::iam::messaging::issue_agent_replay_token_response>::write_json(
    const ores::iam::messaging::issue_agent_replay_token_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::issue_agent_replay_token_response
wire_traits<ores::iam::messaging::issue_agent_replay_token_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::issue_agent_replay_token_response>(text);
}

std::string wire_traits<ores::iam::messaging::issue_agent_replay_token_response>::write_msgpack(
    const ores::iam::messaging::issue_agent_replay_token_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::issue_agent_replay_token_response
wire_traits<ores::iam::messaging::issue_agent_replay_token_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::issue_agent_replay_token_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_agent_held_write_request>::write_json(
    const ores::iam::messaging::get_agent_held_write_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_agent_held_write_request
wire_traits<ores::iam::messaging::get_agent_held_write_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_agent_held_write_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_agent_held_write_request>::write_msgpack(
    const ores::iam::messaging::get_agent_held_write_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_agent_held_write_request
wire_traits<ores::iam::messaging::get_agent_held_write_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_agent_held_write_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_agent_held_write_response>::write_json(
    const ores::iam::messaging::get_agent_held_write_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_agent_held_write_response
wire_traits<ores::iam::messaging::get_agent_held_write_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_agent_held_write_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_agent_held_write_response>::write_msgpack(
    const ores::iam::messaging::get_agent_held_write_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_agent_held_write_response
wire_traits<ores::iam::messaging::get_agent_held_write_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_agent_held_write_response>(data);
}

}
