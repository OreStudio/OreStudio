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
#include "ores.dq/messaging/code_domain_protocol.hpp"

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
// Code Domain helpers
// ============================================================================

void write_code_domain(std::vector<std::byte>& buffer,
    const domain::code_domain& cd) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(cd.version));
    writer::write_string(buffer, cd.code);
    writer::write_string(buffer, cd.name);
    writer::write_string(buffer, cd.description);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(cd.display_order));
    writer::write_string(buffer, cd.modified_by);
    writer::write_string(buffer, cd.performed_by);
    writer::write_string(buffer, cd.change_reason_code);
    writer::write_string(buffer, cd.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(cd.recorded_at));
}

std::expected<domain::code_domain, error_code>
read_code_domain(std::span<const std::byte>& data) {
    domain::code_domain cd;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    cd.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    cd.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    cd.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    cd.description = *description_result;

    auto display_order_result = reader::read_uint32(data);
    if (!display_order_result) return std::unexpected(display_order_result.error());
    cd.display_order = static_cast<int>(*display_order_result);

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    cd.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    cd.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    cd.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    cd.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        cd.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return cd;
}

} // anonymous namespace

// ============================================================================
// Code Domain Messages Implementation
// ============================================================================

std::vector<std::byte> get_code_domains_request::serialize() const {
    return {};
}

std::expected<get_code_domains_request, error_code>
get_code_domains_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_code_domains_request{};
}

std::ostream& operator<<(std::ostream& s, const get_code_domains_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_code_domains_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(domains.size()));
    for (const auto& cd : domains) {
        write_code_domain(buffer, cd);
    }
    return buffer;
}

std::expected<get_code_domains_response, error_code>
get_code_domains_response::deserialize(std::span<const std::byte> data) {
    get_code_domains_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.domains.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_code_domain(data);
        if (!result) return std::unexpected(result.error());
        response.domains.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_code_domains_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_code_domain_request::serialize() const {
    std::vector<std::byte> buffer;
    write_code_domain(buffer, domain);
    return buffer;
}

std::expected<save_code_domain_request, error_code>
save_code_domain_request::deserialize(std::span<const std::byte> data) {
    save_code_domain_request request;

    auto result = read_code_domain(data);
    if (!result) return std::unexpected(result.error());
    request.domain = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_code_domain_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_code_domain_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_code_domain_response, error_code>
save_code_domain_response::deserialize(std::span<const std::byte> data) {
    save_code_domain_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_code_domain_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_code_domain_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_code_domain_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_code_domain_request, error_code>
delete_code_domain_request::deserialize(std::span<const std::byte> data) {
    delete_code_domain_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        request.codes.push_back(*code_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_code_domain_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_code_domain_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_code_domain_response, error_code>
delete_code_domain_response::deserialize(std::span<const std::byte> data) {
    delete_code_domain_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_code_domain_result r;

        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        r.code = *code_result;

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

std::ostream& operator<<(std::ostream& s, const delete_code_domain_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_code_domain_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_code_domain_history_request, error_code>
get_code_domain_history_request::deserialize(std::span<const std::byte> data) {
    get_code_domain_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_code_domain_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_code_domain_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_code_domain(buffer, v);
    }
    return buffer;
}

std::expected<get_code_domain_history_response, error_code>
get_code_domain_history_response::deserialize(std::span<const std::byte> data) {
    get_code_domain_history_response response;

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
        auto result = read_code_domain(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_code_domain_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
