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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep. Must be before rfl/json.hpp
#include "ores.refdata/messaging/business_centre_protocol.hpp"

#include <expected>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"
#include "ores.platform/time/datetime.hpp"

using namespace ores::refdata;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;
using ores::utility::serialization::error_code;

namespace {

/**
 * @brief Helper function to serialize a single business centre.
 */
void serialize_business_centre(std::vector<std::byte>& buffer,
    const domain::business_centre& bc) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(bc.version));
    writer::write_string(buffer, bc.code);
    writer::write_string(buffer, bc.source);
    writer::write_string(buffer, bc.description);
    writer::write_string(buffer, bc.coding_scheme_code);
    writer::write_string(buffer, bc.country_alpha2_code);
    writer::write_bool(buffer, bc.image_id.has_value());
    if (bc.image_id) {
        writer::write_uuid(buffer, *bc.image_id);
    }
    writer::write_string(buffer, bc.recorded_by);
    writer::write_string(buffer, bc.change_reason_code);
    writer::write_string(buffer, bc.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(bc.recorded_at));
}

std::expected<domain::business_centre, ores::utility::serialization::error_code>
deserialize_business_centre(std::span<const std::byte>& data) {
    domain::business_centre bc;

    auto version = reader::read_uint32(data);
    if (!version) return std::unexpected(version.error());
    bc.version = static_cast<int>(*version);

    auto code = reader::read_string(data);
    if (!code) return std::unexpected(code.error());
    bc.code = *code;

    auto source = reader::read_string(data);
    if (!source) return std::unexpected(source.error());
    bc.source = *source;

    auto description = reader::read_string(data);
    if (!description) return std::unexpected(description.error());
    bc.description = *description;

    auto coding_scheme_code = reader::read_string(data);
    if (!coding_scheme_code) return std::unexpected(coding_scheme_code.error());
    bc.coding_scheme_code = *coding_scheme_code;

    auto country_alpha2_code = reader::read_string(data);
    if (!country_alpha2_code) return std::unexpected(country_alpha2_code.error());
    bc.country_alpha2_code = *country_alpha2_code;

    auto has_image_id = reader::read_bool(data);
    if (!has_image_id) return std::unexpected(has_image_id.error());
    if (*has_image_id) {
        auto image_id = reader::read_uuid(data);
        if (!image_id) return std::unexpected(image_id.error());
        bc.image_id = *image_id;
    }

    auto recorded_by = reader::read_string(data);
    if (!recorded_by) return std::unexpected(recorded_by.error());
    bc.recorded_by = *recorded_by;

    auto change_reason_code = reader::read_string(data);
    if (!change_reason_code) return std::unexpected(change_reason_code.error());
    bc.change_reason_code = *change_reason_code;

    auto change_commentary = reader::read_string(data);
    if (!change_commentary) return std::unexpected(change_commentary.error());
    bc.change_commentary = *change_commentary;

    auto recorded_at = reader::read_string(data);
    if (!recorded_at) return std::unexpected(recorded_at.error());
    try {
        bc.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return bc;
}

}

namespace ores::refdata::messaging {

// get_business_centres_request

std::vector<std::byte> get_business_centres_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_business_centres_request, ores::utility::serialization::error_code>
get_business_centres_request::deserialize(std::span<const std::byte> data) {
    get_business_centres_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_business_centres_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_business_centres_response

std::vector<std::byte> get_business_centres_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer, total_available_count);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(business_centres.size()));

    for (const auto& bc : business_centres) {
        serialize_business_centre(buffer, bc);
    }

    return buffer;
}

std::expected<get_business_centres_response, ores::utility::serialization::error_code>
get_business_centres_response::deserialize(std::span<const std::byte> data) {
    get_business_centres_response response;

    auto total_result = reader::read_uint32(data);
    if (!total_result) {
        return std::unexpected(total_result.error());
    }
    response.total_available_count = *total_result;

    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.business_centres.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto bc_result = deserialize_business_centre(data);
        if (!bc_result) {
            return std::unexpected(bc_result.error());
        }
        response.business_centres.push_back(std::move(*bc_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_business_centres_response& v) {
    rfl::json::write(v, s);
    return s;
}

// save_business_centre_request

std::vector<std::byte> save_business_centre_request::serialize() const {
    std::vector<std::byte> buffer;
    serialize_business_centre(buffer, business_centre);
    return buffer;
}

std::expected<save_business_centre_request, ores::utility::serialization::error_code>
save_business_centre_request::deserialize(std::span<const std::byte> data) {
    auto bc_result = deserialize_business_centre(data);
    if (!bc_result) {
        return std::unexpected(bc_result.error());
    }
    return save_business_centre_request{*bc_result};
}

std::ostream& operator<<(std::ostream& s, const save_business_centre_request& v) {
    rfl::json::write(v, s);
    return s;
}

// save_business_centre_response

std::vector<std::byte> save_business_centre_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_business_centre_response, ores::utility::serialization::error_code>
save_business_centre_response::deserialize(std::span<const std::byte> data) {
    save_business_centre_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) {
        return std::unexpected(success_result.error());
    }
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) {
        return std::unexpected(message_result.error());
    }
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_business_centre_response& v) {
    rfl::json::write(v, s);
    return s;
}

// delete_business_centre_request

std::vector<std::byte> delete_business_centre_request::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));

    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }

    return buffer;
}

std::expected<delete_business_centre_request, ores::utility::serialization::error_code>
delete_business_centre_request::deserialize(std::span<const std::byte> data) {
    delete_business_centre_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    request.codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto code_result = reader::read_string(data);
        if (!code_result) {
            return std::unexpected(code_result.error());
        }
        request.codes.push_back(*code_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_business_centre_request& v) {
    rfl::json::write(v, s);
    return s;
}

// delete_business_centre_result

std::ostream& operator<<(std::ostream& s, const delete_business_centre_result& v) {
    rfl::json::write(v, s);
    return s;
}

// delete_business_centre_response

std::vector<std::byte> delete_business_centre_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));

    for (const auto& result : results) {
        writer::write_string(buffer, result.code);
        writer::write_bool(buffer, result.success);
        writer::write_string(buffer, result.message);
    }

    return buffer;
}

std::expected<delete_business_centre_response, ores::utility::serialization::error_code>
delete_business_centre_response::deserialize(std::span<const std::byte> data) {
    delete_business_centre_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_business_centre_result result;

        auto code_result = reader::read_string(data);
        if (!code_result) {
            return std::unexpected(code_result.error());
        }
        result.code = *code_result;

        auto success_result = reader::read_bool(data);
        if (!success_result) {
            return std::unexpected(success_result.error());
        }
        result.success = *success_result;

        auto message_result = reader::read_string(data);
        if (!message_result) {
            return std::unexpected(message_result.error());
        }
        result.message = *message_result;

        response.results.push_back(std::move(result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_business_centre_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_business_centre_history_request

std::vector<std::byte> get_business_centre_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_business_centre_history_request, ores::utility::serialization::error_code>
get_business_centre_history_request::deserialize(std::span<const std::byte> data) {
    get_business_centre_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) {
        return std::unexpected(code_result.error());
    }
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_business_centre_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_business_centre_history_response

std::vector<std::byte> get_business_centre_history_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);

    writer::write_uint32(buffer, static_cast<std::uint32_t>(history.size()));

    for (const auto& bc : history) {
        serialize_business_centre(buffer, bc);
    }

    return buffer;
}

std::expected<get_business_centre_history_response, ores::utility::serialization::error_code>
get_business_centre_history_response::deserialize(std::span<const std::byte> data) {
    get_business_centre_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) {
        return std::unexpected(success_result.error());
    }
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) {
        return std::unexpected(message_result.error());
    }
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    response.history.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto bc_result = deserialize_business_centre(data);
        if (!bc_result) {
            return std::unexpected(bc_result.error());
        }
        response.history.push_back(std::move(*bc_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_business_centre_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
