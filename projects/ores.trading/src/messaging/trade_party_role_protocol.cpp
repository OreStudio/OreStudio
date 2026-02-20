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
#include "ores.trading/messaging/trade_party_role_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::trading::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Trade Party Role helpers
// ============================================================================

void write_trade_party_role(std::vector<std::byte>& buffer,
    const domain::trade_party_role& pr) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(pr.version));
    writer::write_uuid(buffer, pr.id);
    writer::write_uuid(buffer, pr.trade_id);
    writer::write_uuid(buffer, pr.counterparty_id);
    writer::write_string(buffer, pr.role);
    writer::write_string(buffer, pr.modified_by);
    writer::write_string(buffer, pr.performed_by);
    writer::write_string(buffer, pr.change_reason_code);
    writer::write_string(buffer, pr.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(pr.recorded_at));
}

std::expected<domain::trade_party_role, error_code>
read_trade_party_role(std::span<const std::byte>& data) {
    domain::trade_party_role pr;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    pr.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    pr.id = *id_result;

    auto trade_id_result = reader::read_uuid(data);
    if (!trade_id_result) return std::unexpected(trade_id_result.error());
    pr.trade_id = *trade_id_result;

    auto counterparty_id_result = reader::read_uuid(data);
    if (!counterparty_id_result) return std::unexpected(counterparty_id_result.error());
    pr.counterparty_id = *counterparty_id_result;

    auto role_result = reader::read_string(data);
    if (!role_result) return std::unexpected(role_result.error());
    pr.role = *role_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    pr.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    pr.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    pr.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    pr.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        pr.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return pr;
}

} // anonymous namespace

// ============================================================================
// Trade Party Role Messages Implementation
// ============================================================================

std::vector<std::byte> get_trade_party_roles_request::serialize() const {
    return {};
}

std::expected<get_trade_party_roles_request, error_code>
get_trade_party_roles_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_trade_party_roles_request{};
}

std::ostream& operator<<(std::ostream& s, const get_trade_party_roles_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_trade_party_roles_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(roles.size()));
    for (const auto& pr : roles) {
        write_trade_party_role(buffer, pr);
    }
    return buffer;
}

std::expected<get_trade_party_roles_response, error_code>
get_trade_party_roles_response::deserialize(std::span<const std::byte> data) {
    get_trade_party_roles_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.roles.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_trade_party_role(data);
        if (!result) return std::unexpected(result.error());
        response.roles.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_trade_party_roles_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_trade_party_role_request::serialize() const {
    std::vector<std::byte> buffer;
    write_trade_party_role(buffer, role);
    return buffer;
}

std::expected<save_trade_party_role_request, error_code>
save_trade_party_role_request::deserialize(std::span<const std::byte> data) {
    save_trade_party_role_request request;

    auto result = read_trade_party_role(data);
    if (!result) return std::unexpected(result.error());
    request.role = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_trade_party_role_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_trade_party_role_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_trade_party_role_response, error_code>
save_trade_party_role_response::deserialize(std::span<const std::byte> data) {
    save_trade_party_role_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_trade_party_role_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_trade_party_role_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_trade_party_role_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_trade_party_role_request, error_code>
delete_trade_party_role_request::deserialize(std::span<const std::byte> data) {
    delete_trade_party_role_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_trade_party_role_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_trade_party_role_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_trade_party_role_response, error_code>
delete_trade_party_role_response::deserialize(std::span<const std::byte> data) {
    delete_trade_party_role_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_trade_party_role_result r;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        r.id = *id_result;

        auto success_result = reader::read_bool(data);
        if (!success_result) return std::unexpected(success_result.error());
        r.success = *success_result;

        auto message_result = reader::read_string(data);
        if (!message_result) return std::unexpected(message_result.error());
        r.message = *message_result;

        response.results.push_back(std::move(r));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_trade_party_role_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_trade_party_role_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_trade_party_role_history_request, error_code>
get_trade_party_role_history_request::deserialize(std::span<const std::byte> data) {
    get_trade_party_role_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_trade_party_role_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_trade_party_role_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_trade_party_role(buffer, v);
    }
    return buffer;
}

std::expected<get_trade_party_role_history_response, error_code>
get_trade_party_role_history_response::deserialize(std::span<const std::byte> data) {
    get_trade_party_role_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.versions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_trade_party_role(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_trade_party_role_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
