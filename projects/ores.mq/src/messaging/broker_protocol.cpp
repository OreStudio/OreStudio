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
#include "ores.mq/messaging/broker_protocol.hpp"

#include <ostream>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::mq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

// ============================================================================
// register_service_request
// ============================================================================

std::vector<std::byte> register_service_request::serialize() const {
    std::vector<std::byte> buf;
    writer::write_string(buf, service_name);
    writer::write_uint32(buf, static_cast<std::uint32_t>(handled_ranges.size()));
    for (const auto& [min, max] : handled_ranges) {
        writer::write_uint16(buf, min);
        writer::write_uint16(buf, max);
    }
    return buf;
}

std::expected<register_service_request, error_code>
register_service_request::deserialize(std::span<const std::byte> data) {
    register_service_request req;

    auto name = reader::read_string(data);
    if (!name) return std::unexpected(name.error());
    req.service_name = std::move(*name);

    auto count = reader::read_uint32(data);
    if (!count) return std::unexpected(count.error());

    req.handled_ranges.reserve(*count);
    for (std::uint32_t i = 0; i < *count; ++i) {
        auto min = reader::read_uint16(data);
        if (!min) return std::unexpected(min.error());
        auto max = reader::read_uint16(data);
        if (!max) return std::unexpected(max.error());
        req.handled_ranges.emplace_back(*min, *max);
    }
    return req;
}

std::ostream& operator<<(std::ostream& s, const register_service_request& v) {
    s << "{ service_name: " << v.service_name
      << ", ranges: " << v.handled_ranges.size() << " }";
    return s;
}

// ============================================================================
// register_service_response
// ============================================================================

std::vector<std::byte> register_service_response::serialize() const {
    std::vector<std::byte> buf;
    writer::write_bool(buf, success);
    writer::write_string(buf, assigned_id);
    writer::write_string(buf, error_message);
    return buf;
}

std::expected<register_service_response, error_code>
register_service_response::deserialize(std::span<const std::byte> data) {
    register_service_response resp;

    auto ok = reader::read_bool(data);
    if (!ok) return std::unexpected(ok.error());
    resp.success = *ok;

    auto aid = reader::read_string(data);
    if (!aid) return std::unexpected(aid.error());
    resp.assigned_id = std::move(*aid);

    auto err = reader::read_string(data);
    if (!err) return std::unexpected(err.error());
    resp.error_message = std::move(*err);

    return resp;
}

std::ostream& operator<<(std::ostream& s, const register_service_response& v) {
    s << "{ success: " << v.success
      << ", assigned_id: " << v.assigned_id << " }";
    return s;
}

// ============================================================================
// token_refresh_request
// ============================================================================

std::vector<std::byte> token_refresh_request::serialize() const {
    std::vector<std::byte> buf;
    writer::write_string32(buf, expired_jwt);
    return buf;
}

std::expected<token_refresh_request, error_code>
token_refresh_request::deserialize(std::span<const std::byte> data) {
    token_refresh_request req;

    auto jwt = reader::read_string32(data);
    if (!jwt) return std::unexpected(jwt.error());
    req.expired_jwt = std::move(*jwt);

    return req;
}

std::ostream& operator<<(std::ostream& s, const token_refresh_request& v) {
    s << "{ expired_jwt: [" << v.expired_jwt.size() << " chars] }";
    return s;
}

// ============================================================================
// token_refresh_response
// ============================================================================

std::vector<std::byte> token_refresh_response::serialize() const {
    std::vector<std::byte> buf;
    writer::write_bool(buf, success);
    writer::write_string32(buf, new_jwt);
    writer::write_string(buf, error_message);
    return buf;
}

std::expected<token_refresh_response, error_code>
token_refresh_response::deserialize(std::span<const std::byte> data) {
    token_refresh_response resp;

    auto ok = reader::read_bool(data);
    if (!ok) return std::unexpected(ok.error());
    resp.success = *ok;

    auto jwt = reader::read_string32(data);
    if (!jwt) return std::unexpected(jwt.error());
    resp.new_jwt = std::move(*jwt);

    auto err = reader::read_string(data);
    if (!err) return std::unexpected(err.error());
    resp.error_message = std::move(*err);

    return resp;
}

std::ostream& operator<<(std::ostream& s, const token_refresh_response& v) {
    s << "{ success: " << v.success << " }";
    return s;
}

}
