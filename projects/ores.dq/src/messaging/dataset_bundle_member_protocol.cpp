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
#include "ores.dq/messaging/dataset_bundle_member_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::dq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Dataset Bundle Member helpers
// ============================================================================

void write_dataset_bundle_member(std::vector<std::byte>& buffer,
    const domain::dataset_bundle_member& m) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(m.version));
    writer::write_string(buffer, m.bundle_code);
    writer::write_string(buffer, m.dataset_code);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(m.display_order));
    writer::write_bool(buffer, m.optional);
    writer::write_string(buffer, m.change_reason_code);
    writer::write_string(buffer, m.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(m.recorded_at));
}

std::expected<domain::dataset_bundle_member, error_code>
read_dataset_bundle_member(std::span<const std::byte>& data) {
    domain::dataset_bundle_member m;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    m.version = static_cast<int>(*version_result);

    auto bundle_code_result = reader::read_string(data);
    if (!bundle_code_result) return std::unexpected(bundle_code_result.error());
    m.bundle_code = *bundle_code_result;

    auto dataset_code_result = reader::read_string(data);
    if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
    m.dataset_code = *dataset_code_result;

    auto display_order_result = reader::read_uint32(data);
    if (!display_order_result) return std::unexpected(display_order_result.error());
    m.display_order = static_cast<int>(*display_order_result);

    auto optional_result = reader::read_bool(data);
    if (!optional_result) return std::unexpected(optional_result.error());
    m.optional = *optional_result;


    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    m.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    m.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        m.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return m;
}

} // anonymous namespace

// ============================================================================
// Dataset Bundle Member Messages Implementation
// ============================================================================

std::vector<std::byte> get_dataset_bundle_members_request::serialize() const {
    return {};
}

std::expected<get_dataset_bundle_members_request, error_code>
get_dataset_bundle_members_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_dataset_bundle_members_request{};
}

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_dataset_bundle_members_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(members.size()));
    for (const auto& m : members) {
        write_dataset_bundle_member(buffer, m);
    }
    return buffer;
}

std::expected<get_dataset_bundle_members_response, error_code>
get_dataset_bundle_members_response::deserialize(std::span<const std::byte> data) {
    get_dataset_bundle_members_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.members.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_dataset_bundle_member(data);
        if (!result) return std::unexpected(result.error());
        response.members.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_dataset_bundle_members_by_bundle_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, bundle_code);
    return buffer;
}

std::expected<get_dataset_bundle_members_by_bundle_request, error_code>
get_dataset_bundle_members_by_bundle_request::deserialize(std::span<const std::byte> data) {
    get_dataset_bundle_members_by_bundle_request request;

    auto bundle_code_result = reader::read_string(data);
    if (!bundle_code_result) return std::unexpected(bundle_code_result.error());
    request.bundle_code = *bundle_code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_by_bundle_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_dataset_bundle_members_by_bundle_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(members.size()));
    for (const auto& m : members) {
        write_dataset_bundle_member(buffer, m);
    }
    return buffer;
}

std::expected<get_dataset_bundle_members_by_bundle_response, error_code>
get_dataset_bundle_members_by_bundle_response::deserialize(std::span<const std::byte> data) {
    get_dataset_bundle_members_by_bundle_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.members.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_dataset_bundle_member(data);
        if (!result) return std::unexpected(result.error());
        response.members.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_by_bundle_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_dataset_bundle_member_request::serialize() const {
    std::vector<std::byte> buffer;
    write_dataset_bundle_member(buffer, member);
    return buffer;
}

std::expected<save_dataset_bundle_member_request, error_code>
save_dataset_bundle_member_request::deserialize(std::span<const std::byte> data) {
    save_dataset_bundle_member_request request;

    auto result = read_dataset_bundle_member(data);
    if (!result) return std::unexpected(result.error());
    request.member = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_dataset_bundle_member_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_dataset_bundle_member_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_dataset_bundle_member_response, error_code>
save_dataset_bundle_member_response::deserialize(std::span<const std::byte> data) {
    save_dataset_bundle_member_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_dataset_bundle_member_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_member_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const dataset_bundle_member_key& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_dataset_bundle_member_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(keys.size()));
    for (const auto& k : keys) {
        writer::write_string(buffer, k.bundle_code);
        writer::write_string(buffer, k.dataset_code);
    }
    return buffer;
}

std::expected<delete_dataset_bundle_member_request, error_code>
delete_dataset_bundle_member_request::deserialize(std::span<const std::byte> data) {
    delete_dataset_bundle_member_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.keys.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        dataset_bundle_member_key k;

        auto bundle_code_result = reader::read_string(data);
        if (!bundle_code_result) return std::unexpected(bundle_code_result.error());
        k.bundle_code = *bundle_code_result;

        auto dataset_code_result = reader::read_string(data);
        if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
        k.dataset_code = *dataset_code_result;

        request.keys.push_back(std::move(k));
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_member_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_dataset_bundle_member_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.bundle_code);
        writer::write_string(buffer, r.dataset_code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_dataset_bundle_member_response, error_code>
delete_dataset_bundle_member_response::deserialize(std::span<const std::byte> data) {
    delete_dataset_bundle_member_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_dataset_bundle_member_result r;

        auto bundle_code_result = reader::read_string(data);
        if (!bundle_code_result) return std::unexpected(bundle_code_result.error());
        r.bundle_code = *bundle_code_result;

        auto dataset_code_result = reader::read_string(data);
        if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
        r.dataset_code = *dataset_code_result;

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

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_member_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
