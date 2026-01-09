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
#include "ores.variability/messaging/feature_flags_protocol.hpp"

#include <expected>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::variability::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

std::vector<std::byte> list_feature_flags_request::serialize() const {
    return {};
}

std::expected<list_feature_flags_request, ores::utility::serialization::error_code>
list_feature_flags_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(ores::utility::serialization::error_code::payload_too_large);
    }
    return list_feature_flags_request{};
}

std::ostream& operator<<(std::ostream& s, const list_feature_flags_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_feature_flags_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer, static_cast<std::uint32_t>(feature_flags.size()));

    for (const auto& ff : feature_flags) {
        writer::write_string(buffer, ff.name);
        writer::write_bool(buffer, ff.enabled);
        writer::write_string(buffer, ff.description);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(ff.version));
        writer::write_string(buffer, ff.recorded_by);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(ff.recorded_at));
    }

    return buffer;
}

std::expected<list_feature_flags_response, ores::utility::serialization::error_code>
list_feature_flags_response::deserialize(std::span<const std::byte> data) {
    list_feature_flags_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.feature_flags.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::feature_flags ff;

        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        ff.name = *name_result;

        auto enabled_result = reader::read_bool(data);
        if (!enabled_result) return std::unexpected(enabled_result.error());
        ff.enabled = *enabled_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        ff.description = *description_result;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        ff.version = static_cast<int>(*version_result);

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        ff.recorded_by = *recorded_by_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        try {
            ff.recorded_at = ores::platform::time::datetime::parse_time_point(
                *recorded_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }

        response.feature_flags.push_back(std::move(ff));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_feature_flags_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_feature_flag_request::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_string(buffer, flag.name);
    writer::write_bool(buffer, flag.enabled);
    writer::write_string(buffer, flag.description);
    writer::write_string(buffer, flag.recorded_by);

    return buffer;
}

std::expected<save_feature_flag_request, ores::utility::serialization::error_code>
save_feature_flag_request::deserialize(std::span<const std::byte> data) {
    save_feature_flag_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.flag.name = *name_result;

    auto enabled_result = reader::read_bool(data);
    if (!enabled_result) return std::unexpected(enabled_result.error());
    request.flag.enabled = *enabled_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    request.flag.description = *description_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    request.flag.recorded_by = *recorded_by_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_feature_flag_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_feature_flag_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);

    return buffer;
}

std::expected<save_feature_flag_response, ores::utility::serialization::error_code>
save_feature_flag_response::deserialize(std::span<const std::byte> data) {
    save_feature_flag_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_feature_flag_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_feature_flag_request::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_string(buffer, name);

    return buffer;
}

std::expected<delete_feature_flag_request, ores::utility::serialization::error_code>
delete_feature_flag_request::deserialize(std::span<const std::byte> data) {
    delete_feature_flag_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_feature_flag_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_feature_flag_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);

    return buffer;
}

std::expected<delete_feature_flag_response, ores::utility::serialization::error_code>
delete_feature_flag_response::deserialize(std::span<const std::byte> data) {
    delete_feature_flag_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_feature_flag_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_feature_flag_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, name);
    return buffer;
}

std::expected<get_feature_flag_history_request, ores::utility::serialization::error_code>
get_feature_flag_history_request::deserialize(std::span<const std::byte> data) {
    get_feature_flag_history_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_feature_flag_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_feature_flag_history_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(history.size()));

    for (const auto& ff : history) {
        writer::write_string(buffer, ff.name);
        writer::write_bool(buffer, ff.enabled);
        writer::write_string(buffer, ff.description);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(ff.version));
        writer::write_string(buffer, ff.recorded_by);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(ff.recorded_at));
    }

    return buffer;
}

std::expected<get_feature_flag_history_response, ores::utility::serialization::error_code>
get_feature_flag_history_response::deserialize(std::span<const std::byte> data) {
    get_feature_flag_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.history.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::feature_flags ff;

        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        ff.name = *name_result;

        auto enabled_result = reader::read_bool(data);
        if (!enabled_result) return std::unexpected(enabled_result.error());
        ff.enabled = *enabled_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        ff.description = *description_result;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        ff.version = static_cast<int>(*version_result);

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        ff.recorded_by = *recorded_by_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        try {
            ff.recorded_at = ores::platform::time::datetime::parse_time_point(
                *recorded_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }

        response.history.push_back(std::move(ff));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_feature_flag_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
