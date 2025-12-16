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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

namespace ores::variability::messaging {

using namespace ores::comms::messaging;

std::vector<std::byte> list_feature_flags_request::serialize() const {
    return {};
}

std::expected<list_feature_flags_request, comms::messaging::error_code>
list_feature_flags_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(comms::messaging::error_code::payload_too_large);
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
        writer::write_string(buffer, ff.recorded_by);
    }

    return buffer;
}

std::expected<list_feature_flags_response, comms::messaging::error_code>
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

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        ff.recorded_by = *recorded_by_result;

        response.feature_flags.push_back(std::move(ff));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_feature_flags_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
