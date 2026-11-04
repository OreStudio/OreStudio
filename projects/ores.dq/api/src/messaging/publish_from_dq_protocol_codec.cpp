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
#include "ores.dq.api/messaging/publish_from_dq_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::publish_from_dq_command>::write_json(
    const ores::dq::messaging::publish_from_dq_command& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_from_dq_command
wire_traits<ores::dq::messaging::publish_from_dq_command>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_from_dq_command>(text);
}

std::string wire_traits<ores::dq::messaging::publish_from_dq_command>::write_msgpack(
    const ores::dq::messaging::publish_from_dq_command& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_from_dq_command
wire_traits<ores::dq::messaging::publish_from_dq_command>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_from_dq_command>(data);
}

std::string wire_traits<ores::dq::messaging::publish_from_dq_result>::write_json(
    const ores::dq::messaging::publish_from_dq_result& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_from_dq_result
wire_traits<ores::dq::messaging::publish_from_dq_result>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_from_dq_result>(text);
}

std::string wire_traits<ores::dq::messaging::publish_from_dq_result>::write_msgpack(
    const ores::dq::messaging::publish_from_dq_result& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_from_dq_result
wire_traits<ores::dq::messaging::publish_from_dq_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_from_dq_result>(data);
}

std::string wire_traits<ores::dq::messaging::publish_ip2country_from_dq_request>::write_json(
    const ores::dq::messaging::publish_ip2country_from_dq_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_ip2country_from_dq_request
wire_traits<ores::dq::messaging::publish_ip2country_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_ip2country_from_dq_request>(text);
}

std::string wire_traits<ores::dq::messaging::publish_ip2country_from_dq_request>::write_msgpack(
    const ores::dq::messaging::publish_ip2country_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_ip2country_from_dq_request
wire_traits<ores::dq::messaging::publish_ip2country_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_ip2country_from_dq_request>(data);
}

std::string wire_traits<ores::dq::messaging::publish_coding_schemes_from_dq_request>::write_json(
    const ores::dq::messaging::publish_coding_schemes_from_dq_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_coding_schemes_from_dq_request
wire_traits<ores::dq::messaging::publish_coding_schemes_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_coding_schemes_from_dq_request>(text);
}

std::string wire_traits<ores::dq::messaging::publish_coding_schemes_from_dq_request>::write_msgpack(
    const ores::dq::messaging::publish_coding_schemes_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_coding_schemes_from_dq_request
wire_traits<ores::dq::messaging::publish_coding_schemes_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_coding_schemes_from_dq_request>(data);
}

std::string wire_traits<ores::dq::messaging::publish_badge_severities_from_dq_request>::write_json(
    const ores::dq::messaging::publish_badge_severities_from_dq_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_badge_severities_from_dq_request
wire_traits<ores::dq::messaging::publish_badge_severities_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_badge_severities_from_dq_request>(text);
}

std::string
wire_traits<ores::dq::messaging::publish_badge_severities_from_dq_request>::write_msgpack(
    const ores::dq::messaging::publish_badge_severities_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_badge_severities_from_dq_request
wire_traits<ores::dq::messaging::publish_badge_severities_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_badge_severities_from_dq_request>(
        data);
}

std::string wire_traits<ores::dq::messaging::publish_badge_definitions_from_dq_request>::write_json(
    const ores::dq::messaging::publish_badge_definitions_from_dq_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_badge_definitions_from_dq_request
wire_traits<ores::dq::messaging::publish_badge_definitions_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_badge_definitions_from_dq_request>(text);
}

std::string
wire_traits<ores::dq::messaging::publish_badge_definitions_from_dq_request>::write_msgpack(
    const ores::dq::messaging::publish_badge_definitions_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_badge_definitions_from_dq_request
wire_traits<ores::dq::messaging::publish_badge_definitions_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_badge_definitions_from_dq_request>(
        data);
}

std::string wire_traits<ores::dq::messaging::publish_code_domains_from_dq_request>::write_json(
    const ores::dq::messaging::publish_code_domains_from_dq_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_code_domains_from_dq_request
wire_traits<ores::dq::messaging::publish_code_domains_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_code_domains_from_dq_request>(text);
}

std::string wire_traits<ores::dq::messaging::publish_code_domains_from_dq_request>::write_msgpack(
    const ores::dq::messaging::publish_code_domains_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_code_domains_from_dq_request
wire_traits<ores::dq::messaging::publish_code_domains_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_code_domains_from_dq_request>(data);
}

std::string wire_traits<ores::dq::messaging::publish_badge_mappings_from_dq_request>::write_json(
    const ores::dq::messaging::publish_badge_mappings_from_dq_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::publish_badge_mappings_from_dq_request
wire_traits<ores::dq::messaging::publish_badge_mappings_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::publish_badge_mappings_from_dq_request>(text);
}

std::string wire_traits<ores::dq::messaging::publish_badge_mappings_from_dq_request>::write_msgpack(
    const ores::dq::messaging::publish_badge_mappings_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publish_badge_mappings_from_dq_request
wire_traits<ores::dq::messaging::publish_badge_mappings_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publish_badge_mappings_from_dq_request>(data);
}

}
