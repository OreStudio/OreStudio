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
#include "ores.iam.api/domain/agent_key_format.hpp"
#include <algorithm>
#include <array>
#include <cstdint>

namespace ores::iam::domain {

namespace {

constexpr std::size_t id_digits = 32;
constexpr std::size_t secret_digits = 64;
constexpr std::size_t key_length =
    agent_key_prefix.size() + id_digits + 1 + id_digits + 1 + secret_digits;

bool is_lower_hex(std::string_view text) {
    return std::ranges::all_of(
        text, [](char c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'); });
}

std::uint8_t hex_value(char c) {
    return static_cast<std::uint8_t>(c <= '9' ? c - '0' : c - 'a' + 10);
}

boost::uuids::uuid uuid_of_hex(std::string_view hex) {
    boost::uuids::uuid id{};
    for (std::size_t i = 0; i < id.size(); ++i)
        id.data[i] =
            static_cast<std::uint8_t>((hex_value(hex[2 * i]) << 4) | hex_value(hex[2 * i + 1]));
    return id;
}

std::string hex_of_uuid(const boost::uuids::uuid& id) {
    static constexpr std::array<char, 16> digits = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
    std::string out;
    out.reserve(id_digits);
    for (const auto byte : id) {
        out.push_back(digits[byte >> 4]);
        out.push_back(digits[byte & 0x0F]);
    }
    return out;
}

}

bool is_agent_key(std::string_view value) {
    return value.starts_with(agent_key_prefix);
}

std::optional<agent_key_parts> parse_agent_key(std::string_view value) {
    if (value.size() != key_length || !is_agent_key(value))
        return std::nullopt;
    auto rest = value.substr(agent_key_prefix.size());
    const auto tenant = rest.substr(0, id_digits);
    const auto key = rest.substr(id_digits + 1, id_digits);
    const auto secret = rest.substr(2 * id_digits + 2);
    if (rest[id_digits] != '_' || rest[2 * id_digits + 1] != '_')
        return std::nullopt;
    if (!is_lower_hex(tenant) || !is_lower_hex(key) || !is_lower_hex(secret))
        return std::nullopt;
    return agent_key_parts{.tenant_id = uuid_of_hex(tenant),
                           .key_id = uuid_of_hex(key),
                           .secret = std::string(secret)};
}

std::string format_agent_key(const agent_key_parts& parts) {
    return std::string(agent_key_prefix) + hex_of_uuid(parts.tenant_id) + "_" +
           hex_of_uuid(parts.key_id) + "_" + parts.secret;
}

}
