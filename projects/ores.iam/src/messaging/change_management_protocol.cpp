/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/messaging/change_management_protocol.hpp"

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

// get_change_reason_categories_request

std::vector<std::byte> get_change_reason_categories_request::serialize() const {
    return {};
}

std::expected<get_change_reason_categories_request,
              ores::utility::serialization::error_code>
get_change_reason_categories_request::deserialize(
    std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_change_reason_categories_request{};
}

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_categories_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_change_reason_categories_response

std::vector<std::byte>
get_change_reason_categories_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(categories.size()));

    for (const auto& c : categories) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(c.version));
        writer::write_string(buffer, c.code);
        writer::write_string(buffer, c.description);
        writer::write_string(buffer, c.recorded_by);
        writer::write_string(buffer, c.change_commentary);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(c.recorded_at));
    }

    return buffer;
}

std::expected<get_change_reason_categories_response,
              ores::utility::serialization::error_code>
get_change_reason_categories_response::deserialize(
    std::span<const std::byte> data) {
    get_change_reason_categories_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.categories.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::change_reason_category c;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        c.version = static_cast<int>(*version_result);

        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        c.code = *code_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        c.description = *description_result;

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        c.recorded_by = *recorded_by_result;

        auto change_commentary_result = reader::read_string(data);
        if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
        c.change_commentary = *change_commentary_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        try {
            c.recorded_at = ores::platform::time::datetime::parse_time_point(
                *recorded_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }

        response.categories.push_back(std::move(c));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_categories_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_change_reasons_request

std::vector<std::byte> get_change_reasons_request::serialize() const {
    return {};
}

std::expected<get_change_reasons_request,
              ores::utility::serialization::error_code>
get_change_reasons_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_change_reasons_request{};
}

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_change_reasons_response

std::vector<std::byte> get_change_reasons_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(reasons.size()));

    for (const auto& r : reasons) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(r.version));
        writer::write_string(buffer, r.code);
        writer::write_string(buffer, r.description);
        writer::write_string(buffer, r.category_code);
        writer::write_bool(buffer, r.applies_to_amend);
        writer::write_bool(buffer, r.applies_to_delete);
        writer::write_bool(buffer, r.requires_commentary);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(r.display_order));
        writer::write_string(buffer, r.recorded_by);
        writer::write_string(buffer, r.change_commentary);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(r.recorded_at));
    }

    return buffer;
}

std::expected<get_change_reasons_response,
              ores::utility::serialization::error_code>
get_change_reasons_response::deserialize(std::span<const std::byte> data) {
    get_change_reasons_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.reasons.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::change_reason r;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        r.version = static_cast<int>(*version_result);

        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        r.code = *code_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        r.description = *description_result;

        auto category_code_result = reader::read_string(data);
        if (!category_code_result) return std::unexpected(category_code_result.error());
        r.category_code = *category_code_result;

        auto applies_to_amend_result = reader::read_bool(data);
        if (!applies_to_amend_result) return std::unexpected(applies_to_amend_result.error());
        r.applies_to_amend = *applies_to_amend_result;

        auto applies_to_delete_result = reader::read_bool(data);
        if (!applies_to_delete_result) return std::unexpected(applies_to_delete_result.error());
        r.applies_to_delete = *applies_to_delete_result;

        auto requires_commentary_result = reader::read_bool(data);
        if (!requires_commentary_result) return std::unexpected(requires_commentary_result.error());
        r.requires_commentary = *requires_commentary_result;

        auto display_order_result = reader::read_uint32(data);
        if (!display_order_result) return std::unexpected(display_order_result.error());
        r.display_order = static_cast<int>(*display_order_result);

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        r.recorded_by = *recorded_by_result;

        auto change_commentary_result = reader::read_string(data);
        if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
        r.change_commentary = *change_commentary_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        try {
            r.recorded_at = ores::platform::time::datetime::parse_time_point(
                *recorded_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }

        response.reasons.push_back(std::move(r));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_change_reasons_by_category_request

std::vector<std::byte>
get_change_reasons_by_category_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, category_code);
    return buffer;
}

std::expected<get_change_reasons_by_category_request,
              ores::utility::serialization::error_code>
get_change_reasons_by_category_request::deserialize(
    std::span<const std::byte> data) {
    get_change_reasons_by_category_request request;

    auto category_code_result = reader::read_string(data);
    if (!category_code_result)
        return std::unexpected(category_code_result.error());
    request.category_code = *category_code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_by_category_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_change_reasons_by_category_response

std::vector<std::byte>
get_change_reasons_by_category_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(reasons.size()));

    for (const auto& r : reasons) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(r.version));
        writer::write_string(buffer, r.code);
        writer::write_string(buffer, r.description);
        writer::write_string(buffer, r.category_code);
        writer::write_bool(buffer, r.applies_to_amend);
        writer::write_bool(buffer, r.applies_to_delete);
        writer::write_bool(buffer, r.requires_commentary);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(r.display_order));
        writer::write_string(buffer, r.recorded_by);
        writer::write_string(buffer, r.change_commentary);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(r.recorded_at));
    }

    return buffer;
}

std::expected<get_change_reasons_by_category_response,
              ores::utility::serialization::error_code>
get_change_reasons_by_category_response::deserialize(
    std::span<const std::byte> data) {
    get_change_reasons_by_category_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.reasons.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::change_reason r;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        r.version = static_cast<int>(*version_result);

        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        r.code = *code_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        r.description = *description_result;

        auto category_code_result = reader::read_string(data);
        if (!category_code_result) return std::unexpected(category_code_result.error());
        r.category_code = *category_code_result;

        auto applies_to_amend_result = reader::read_bool(data);
        if (!applies_to_amend_result) return std::unexpected(applies_to_amend_result.error());
        r.applies_to_amend = *applies_to_amend_result;

        auto applies_to_delete_result = reader::read_bool(data);
        if (!applies_to_delete_result) return std::unexpected(applies_to_delete_result.error());
        r.applies_to_delete = *applies_to_delete_result;

        auto requires_commentary_result = reader::read_bool(data);
        if (!requires_commentary_result) return std::unexpected(requires_commentary_result.error());
        r.requires_commentary = *requires_commentary_result;

        auto display_order_result = reader::read_uint32(data);
        if (!display_order_result) return std::unexpected(display_order_result.error());
        r.display_order = static_cast<int>(*display_order_result);

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        r.recorded_by = *recorded_by_result;

        auto change_commentary_result = reader::read_string(data);
        if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
        r.change_commentary = *change_commentary_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        try {
            r.recorded_at = ores::platform::time::datetime::parse_time_point(
                *recorded_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }

        response.reasons.push_back(std::move(r));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_by_category_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
