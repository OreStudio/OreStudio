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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep. Must be before rfl/json.hpp
#include "ores.risk/messaging/country_protocol.hpp"

#include <expected>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"
#include "ores.platform/time/datetime.hpp"

using namespace ores::risk;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;
using ores::utility::serialization::error_code;

namespace {

/**
 * @brief Helper function to serialize a single country
 */
void serialize_country(std::vector<std::byte>& buffer, const domain::country& country) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(country.version));
    writer::write_string(buffer, country.alpha2_code);
    writer::write_string(buffer, country.alpha3_code);
    writer::write_string(buffer, country.numeric_code);
    writer::write_string(buffer, country.name);
    writer::write_string(buffer, country.official_name);
    // Write optional image_id: bool flag followed by UUID if present
    writer::write_bool(buffer, country.image_id.has_value());
    if (country.image_id) {
        writer::write_uuid(buffer, *country.image_id);
    }
    writer::write_string(buffer, country.recorded_by);
    writer::write_string(buffer, country.change_reason_code);
    writer::write_string(buffer, country.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(country.recorded_at));
}

std::expected<domain::country, ores::utility::serialization::error_code>
deserialize_country(std::span<const std::byte>& data) {
    domain::country country;

    auto version = reader::read_uint32(data);
    if (!version) return std::unexpected(version.error());
    country.version = static_cast<int>(*version);

    auto alpha2_code = reader::read_string(data);
    if (!alpha2_code) return std::unexpected(alpha2_code.error());
    country.alpha2_code = *alpha2_code;

    auto alpha3_code = reader::read_string(data);
    if (!alpha3_code) return std::unexpected(alpha3_code.error());
    country.alpha3_code = *alpha3_code;

    auto numeric_code = reader::read_string(data);
    if (!numeric_code) return std::unexpected(numeric_code.error());
    country.numeric_code = *numeric_code;

    auto name = reader::read_string(data);
    if (!name) return std::unexpected(name.error());
    country.name = *name;

    auto official_name = reader::read_string(data);
    if (!official_name) return std::unexpected(official_name.error());
    country.official_name = *official_name;

    // Read optional image_id: bool flag followed by UUID if present
    auto has_image_id = reader::read_bool(data);
    if (!has_image_id) return std::unexpected(has_image_id.error());
    if (*has_image_id) {
        auto image_id = reader::read_uuid(data);
        if (!image_id) return std::unexpected(image_id.error());
        country.image_id = *image_id;
    }

    auto recorded_by = reader::read_string(data);
    if (!recorded_by) return std::unexpected(recorded_by.error());
    country.recorded_by = *recorded_by;

    auto change_reason_code = reader::read_string(data);
    if (!change_reason_code) return std::unexpected(change_reason_code.error());
    country.change_reason_code = *change_reason_code;

    auto change_commentary = reader::read_string(data);
    if (!change_commentary) return std::unexpected(change_commentary.error());
    country.change_commentary = *change_commentary;

    auto recorded_at = reader::read_string(data);
    if (!recorded_at) return std::unexpected(recorded_at.error());
    try {
        country.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return country;
}

}

namespace ores::risk::messaging {

// get_countries_request

std::vector<std::byte> get_countries_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_countries_request, ores::utility::serialization::error_code>
get_countries_request::deserialize(std::span<const std::byte> data) {
    get_countries_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_countries_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_countries_response

std::vector<std::byte> get_countries_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write country count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(countries.size()));

    // Write each country using the serialize helper
    for (const auto& country : countries) {
        serialize_country(buffer, country);
    }

    return buffer;
}

std::expected<get_countries_response, ores::utility::serialization::error_code>
get_countries_response::deserialize(std::span<const std::byte> data) {
    get_countries_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) {
        return std::unexpected(total_result.error());
    }
    response.total_available_count = *total_result;

    // Read country count in this response
    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    // Read each country using the deserialize helper
    response.countries.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto country_result = deserialize_country(data);
        if (!country_result) {
            return std::unexpected(country_result.error());
        }
        response.countries.push_back(std::move(*country_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_countries_response& v) {
    rfl::json::write(v, s);
    return s;
}

// save_country_request

std::vector<std::byte> save_country_request::serialize() const {
    std::vector<std::byte> buffer;
    serialize_country(buffer, country);
    return buffer;
}

std::expected<save_country_request, ores::utility::serialization::error_code>
save_country_request::deserialize(std::span<const std::byte> data) {
    auto country_result = deserialize_country(data);
    if (!country_result) {
        return std::unexpected(country_result.error());
    }
    return save_country_request{*country_result};
}

std::ostream& operator<<(std::ostream& s, const save_country_request& v) {
    rfl::json::write(v, s);
    return s;
}

// save_country_response

std::vector<std::byte> save_country_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_country_response, ores::utility::serialization::error_code>
save_country_response::deserialize(std::span<const std::byte> data) {
    save_country_response response;

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

std::ostream& operator<<(std::ostream& s, const save_country_response& v) {
    rfl::json::write(v, s);
    return s;
}

// delete_country_request

std::vector<std::byte> delete_country_request::serialize() const {
    std::vector<std::byte> buffer;

    // Write count of alpha-2 codes
    writer::write_uint32(buffer, static_cast<std::uint32_t>(alpha2_codes.size()));

    // Write each alpha-2 code
    for (const auto& alpha2_code : alpha2_codes) {
        writer::write_string(buffer, alpha2_code);
    }

    return buffer;
}

std::expected<delete_country_request, ores::utility::serialization::error_code>
delete_country_request::deserialize(std::span<const std::byte> data) {
    delete_country_request request;

    // Read count
    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    // Read each alpha-2 code
    request.alpha2_codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto alpha2_code_result = reader::read_string(data);
        if (!alpha2_code_result) {
            return std::unexpected(alpha2_code_result.error());
        }
        request.alpha2_codes.push_back(*alpha2_code_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_country_request& v) {
    rfl::json::write(v, s);
    return s;
}

// delete_country_result

std::ostream& operator<<(std::ostream& s, const delete_country_result& v) {
    rfl::json::write(v, s);
    return s;
}

// delete_country_response

std::vector<std::byte> delete_country_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write count of results
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));

    // Write each result
    for (const auto& result : results) {
        writer::write_string(buffer, result.alpha2_code);
        writer::write_bool(buffer, result.success);
        writer::write_string(buffer, result.message);
    }

    return buffer;
}

std::expected<delete_country_response, ores::utility::serialization::error_code>
delete_country_response::deserialize(std::span<const std::byte> data) {
    delete_country_response response;

    // Read count
    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    // Read each result
    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_country_result result;

        auto alpha2_code_result = reader::read_string(data);
        if (!alpha2_code_result) {
            return std::unexpected(alpha2_code_result.error());
        }
        result.alpha2_code = *alpha2_code_result;

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

std::ostream& operator<<(std::ostream& s, const delete_country_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_country_history_request

std::vector<std::byte> get_country_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, alpha2_code);
    return buffer;
}

std::expected<get_country_history_request, ores::utility::serialization::error_code>
get_country_history_request::deserialize(std::span<const std::byte> data) {
    get_country_history_request request;

    auto alpha2_code_result = reader::read_string(data);
    if (!alpha2_code_result) {
        return std::unexpected(alpha2_code_result.error());
    }
    request.alpha2_code = *alpha2_code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_country_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_country_history_response

std::vector<std::byte> get_country_history_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);

    // Write count of history entries
    writer::write_uint32(buffer, static_cast<std::uint32_t>(history.size()));

    // Write each country version
    for (const auto& country : history) {
        serialize_country(buffer, country);
    }

    return buffer;
}

std::expected<get_country_history_response, ores::utility::serialization::error_code>
get_country_history_response::deserialize(std::span<const std::byte> data) {
    get_country_history_response response;

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

    // Read count of history entries
    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    // Read each country version
    response.history.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto country_result = deserialize_country(data);
        if (!country_result) {
            return std::unexpected(country_result.error());
        }
        response.history.push_back(std::move(*country_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_country_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
