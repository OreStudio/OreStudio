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
#include "ores.variability.api/messaging/operations_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::variability::messaging::clear_bootstrap_mode_request>::write_json(
    const ores::variability::messaging::clear_bootstrap_mode_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::clear_bootstrap_mode_request
wire_traits<ores::variability::messaging::clear_bootstrap_mode_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::clear_bootstrap_mode_request>(text);
}

std::string wire_traits<ores::variability::messaging::clear_bootstrap_mode_request>::write_msgpack(
    const ores::variability::messaging::clear_bootstrap_mode_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::clear_bootstrap_mode_request
wire_traits<ores::variability::messaging::clear_bootstrap_mode_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::clear_bootstrap_mode_request>(data);
}

std::string wire_traits<ores::variability::messaging::clear_bootstrap_mode_response>::write_json(
    const ores::variability::messaging::clear_bootstrap_mode_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::clear_bootstrap_mode_response
wire_traits<ores::variability::messaging::clear_bootstrap_mode_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::clear_bootstrap_mode_response>(text);
}

std::string wire_traits<ores::variability::messaging::clear_bootstrap_mode_response>::write_msgpack(
    const ores::variability::messaging::clear_bootstrap_mode_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::clear_bootstrap_mode_response
wire_traits<ores::variability::messaging::clear_bootstrap_mode_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::clear_bootstrap_mode_response>(data);
}

std::string
wire_traits<ores::variability::messaging::complete_system_onboarding_request>::write_json(
    const ores::variability::messaging::complete_system_onboarding_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::complete_system_onboarding_request
wire_traits<ores::variability::messaging::complete_system_onboarding_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::complete_system_onboarding_request>(
        text);
}

std::string
wire_traits<ores::variability::messaging::complete_system_onboarding_request>::write_msgpack(
    const ores::variability::messaging::complete_system_onboarding_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::complete_system_onboarding_request
wire_traits<ores::variability::messaging::complete_system_onboarding_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::complete_system_onboarding_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::complete_system_onboarding_response>::write_json(
    const ores::variability::messaging::complete_system_onboarding_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::complete_system_onboarding_response
wire_traits<ores::variability::messaging::complete_system_onboarding_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::complete_system_onboarding_response>(
        text);
}

std::string
wire_traits<ores::variability::messaging::complete_system_onboarding_response>::write_msgpack(
    const ores::variability::messaging::complete_system_onboarding_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::complete_system_onboarding_response
wire_traits<ores::variability::messaging::complete_system_onboarding_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::complete_system_onboarding_response>(
        data);
}

std::string
wire_traits<ores::variability::messaging::complete_party_onboarding_request>::write_json(
    const ores::variability::messaging::complete_party_onboarding_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::complete_party_onboarding_request
wire_traits<ores::variability::messaging::complete_party_onboarding_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::complete_party_onboarding_request>(text);
}

std::string
wire_traits<ores::variability::messaging::complete_party_onboarding_request>::write_msgpack(
    const ores::variability::messaging::complete_party_onboarding_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::complete_party_onboarding_request
wire_traits<ores::variability::messaging::complete_party_onboarding_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::complete_party_onboarding_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::complete_party_onboarding_response>::write_json(
    const ores::variability::messaging::complete_party_onboarding_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::complete_party_onboarding_response
wire_traits<ores::variability::messaging::complete_party_onboarding_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::complete_party_onboarding_response>(
        text);
}

std::string
wire_traits<ores::variability::messaging::complete_party_onboarding_response>::write_msgpack(
    const ores::variability::messaging::complete_party_onboarding_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::complete_party_onboarding_response
wire_traits<ores::variability::messaging::complete_party_onboarding_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::complete_party_onboarding_response>(
        data);
}

std::string wire_traits<ores::variability::messaging::get_setting_request>::write_json(
    const ores::variability::messaging::get_setting_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_setting_request
wire_traits<ores::variability::messaging::get_setting_request>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::get_setting_request>(text);
}

std::string wire_traits<ores::variability::messaging::get_setting_request>::write_msgpack(
    const ores::variability::messaging::get_setting_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_setting_request
wire_traits<ores::variability::messaging::get_setting_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_setting_request>(data);
}

std::string wire_traits<ores::variability::messaging::get_setting_response>::write_json(
    const ores::variability::messaging::get_setting_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_setting_response
wire_traits<ores::variability::messaging::get_setting_response>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::get_setting_response>(text);
}

std::string wire_traits<ores::variability::messaging::get_setting_response>::write_msgpack(
    const ores::variability::messaging::get_setting_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_setting_response
wire_traits<ores::variability::messaging::get_setting_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_setting_response>(data);
}

}
