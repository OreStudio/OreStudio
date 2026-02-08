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
#include "ores.iam/messaging/account_party_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::iam::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Account Party helpers
// ============================================================================

void write_account_party(std::vector<std::byte>& buffer,
    const domain::account_party& ap) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ap.version));
    writer::write_uuid(buffer, ap.account_id);
    writer::write_uuid(buffer, ap.party_id);
    writer::write_string(buffer, ap.recorded_by);
    writer::write_string(buffer, ap.performed_by);
    writer::write_string(buffer, ap.change_reason_code);
    writer::write_string(buffer, ap.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(ap.recorded_at));
}

std::expected<domain::account_party, error_code>
read_account_party(std::span<const std::byte>& data) {
    domain::account_party ap;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    ap.version = static_cast<int>(*version_result);

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    ap.account_id = *account_id_result;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    ap.party_id = *party_id_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    ap.recorded_by = *recorded_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    ap.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    ap.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    ap.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        ap.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return ap;
}

} // anonymous namespace

// ============================================================================
// Account Party Messages Implementation
// ============================================================================

std::vector<std::byte> get_account_parties_request::serialize() const {
    return {};
}

std::expected<get_account_parties_request, error_code>
get_account_parties_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_account_parties_request{};
}

std::ostream& operator<<(std::ostream& s, const get_account_parties_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_account_parties_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(account_parties.size()));
    for (const auto& ap : account_parties) {
        write_account_party(buffer, ap);
    }
    return buffer;
}

std::expected<get_account_parties_response, error_code>
get_account_parties_response::deserialize(std::span<const std::byte> data) {
    get_account_parties_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.account_parties.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_account_party(data);
        if (!result) return std::unexpected(result.error());
        response.account_parties.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_account_parties_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_account_parties_by_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<get_account_parties_by_account_request, error_code>
get_account_parties_by_account_request::deserialize(std::span<const std::byte> data) {
    get_account_parties_by_account_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_account_parties_by_account_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_account_parties_by_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(account_parties.size()));
    for (const auto& ap : account_parties) {
        write_account_party(buffer, ap);
    }
    return buffer;
}

std::expected<get_account_parties_by_account_response, error_code>
get_account_parties_by_account_response::deserialize(std::span<const std::byte> data) {
    get_account_parties_by_account_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.account_parties.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_account_party(data);
        if (!result) return std::unexpected(result.error());
        response.account_parties.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_account_parties_by_account_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_account_party_request::serialize() const {
    std::vector<std::byte> buffer;
    write_account_party(buffer, account_party);
    return buffer;
}

std::expected<save_account_party_request, error_code>
save_account_party_request::deserialize(std::span<const std::byte> data) {
    save_account_party_request request;

    auto result = read_account_party(data);
    if (!result) return std::unexpected(result.error());
    request.account_party = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_account_party_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_account_party_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_account_party_response, error_code>
save_account_party_response::deserialize(std::span<const std::byte> data) {
    save_account_party_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_account_party_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_account_party_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const account_party_key& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_account_party_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(keys.size()));
    for (const auto& k : keys) {
        writer::write_uuid(buffer, k.account_id);
        writer::write_uuid(buffer, k.party_id);
    }
    return buffer;
}

std::expected<delete_account_party_request, error_code>
delete_account_party_request::deserialize(std::span<const std::byte> data) {
    delete_account_party_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.keys.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        account_party_key k;

        auto account_id_result = reader::read_uuid(data);
        if (!account_id_result) return std::unexpected(account_id_result.error());
        k.account_id = *account_id_result;

        auto party_id_result = reader::read_uuid(data);
        if (!party_id_result) return std::unexpected(party_id_result.error());
        k.party_id = *party_id_result;

        request.keys.push_back(std::move(k));
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_account_party_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_account_party_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.account_id);
        writer::write_uuid(buffer, r.party_id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_account_party_response, error_code>
delete_account_party_response::deserialize(std::span<const std::byte> data) {
    delete_account_party_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_account_party_result r;

        auto account_id_result = reader::read_uuid(data);
        if (!account_id_result) return std::unexpected(account_id_result.error());
        r.account_id = *account_id_result;

        auto party_id_result = reader::read_uuid(data);
        if (!party_id_result) return std::unexpected(party_id_result.error());
        r.party_id = *party_id_result;

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

std::ostream& operator<<(std::ostream& s, const delete_account_party_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
