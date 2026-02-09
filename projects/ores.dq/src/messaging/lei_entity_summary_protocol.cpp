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
#include "ores.dq/messaging/lei_entity_summary_protocol.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::dq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

// ============================================================================
// lei_entity_summary
// ============================================================================

std::vector<std::byte> lei_entity_summary::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, lei);
    writer::write_string(buffer, entity_legal_name);
    writer::write_string(buffer, country);
    writer::write_string(buffer, entity_status);
    return buffer;
}

std::expected<lei_entity_summary, error_code>
lei_entity_summary::deserialize(std::span<const std::byte>& data) {
    lei_entity_summary result;

    auto lei_result = reader::read_string(data);
    if (!lei_result) return std::unexpected(lei_result.error());
    result.lei = *lei_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    result.entity_legal_name = *name_result;

    auto country_result = reader::read_string(data);
    if (!country_result) return std::unexpected(country_result.error());
    result.country = *country_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    result.entity_status = *status_result;

    return result;
}

std::ostream& operator<<(std::ostream& s, const lei_entity_summary& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_lei_entities_summary_request
// ============================================================================

std::vector<std::byte> get_lei_entities_summary_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, search_filter);
    return buffer;
}

std::expected<get_lei_entities_summary_request, error_code>
get_lei_entities_summary_request::deserialize(std::span<const std::byte> data) {
    get_lei_entities_summary_request request;

    auto filter_result = reader::read_string(data);
    if (!filter_result) return std::unexpected(filter_result.error());
    request.search_filter = *filter_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_lei_entities_summary_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_lei_entities_summary_response
// ============================================================================

std::vector<std::byte> get_lei_entities_summary_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);

    writer::write_uint32(buffer, static_cast<std::uint32_t>(entities.size()));
    for (const auto& entity : entities) {
        auto entity_bytes = entity.serialize();
        buffer.insert(buffer.end(), entity_bytes.begin(), entity_bytes.end());
    }

    return buffer;
}

std::expected<get_lei_entities_summary_response, error_code>
get_lei_entities_summary_response::deserialize(std::span<const std::byte> data) {
    get_lei_entities_summary_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.entities.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto entity = lei_entity_summary::deserialize(data);
        if (!entity) return std::unexpected(entity.error());
        response.entities.push_back(std::move(*entity));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_lei_entities_summary_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
