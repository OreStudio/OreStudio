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
#include "ores.refdata/messaging/currency_history_protocol.hpp"

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
 * @brief Helper function to serialize a single currency version
 */
void serialize_currency_version(std::vector<std::byte>& buffer, const domain::currency_version& version) {
    // Write currency data
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.data.version));
    writer::write_string(buffer, version.data.iso_code);
    writer::write_string(buffer, version.data.name);
    writer::write_string(buffer, version.data.numeric_code);
    writer::write_string(buffer, version.data.symbol);
    writer::write_string(buffer, version.data.fraction_symbol);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.data.fractions_per_unit));
    writer::write_string(buffer, version.data.rounding_type);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.data.rounding_precision));
    writer::write_string(buffer, version.data.format);
    writer::write_string(buffer, version.data.monetary_nature);
    writer::write_string(buffer, version.data.market_tier);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(version.data.recorded_at));

    // Write optional image_id: bool flag followed by UUID if present
    writer::write_bool(buffer, version.data.image_id.has_value());
    if (version.data.image_id) {
        writer::write_uuid(buffer, *version.data.image_id);
    }

    // Write version metadata
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.version_number));
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(version.recorded_at));
    writer::write_string(buffer, version.change_summary);
}

std::expected<domain::currency_version, ores::utility::serialization::error_code>
deserialize_currency_version(std::span<const std::byte>& data) {
    domain::currency_version version;

    // Read currency data
    auto db_version = reader::read_uint32(data);
    if (!db_version) return std::unexpected(db_version.error());
    version.data.version = static_cast<int>(*db_version);

    auto iso_code = reader::read_string(data);
    if (!iso_code) return std::unexpected(iso_code.error());
    version.data.iso_code = *iso_code;

    auto name = reader::read_string(data);
    if (!name) return std::unexpected(name.error());
    version.data.name = *name;

    auto numeric_code = reader::read_string(data);
    if (!numeric_code) return std::unexpected(numeric_code.error());
    version.data.numeric_code = *numeric_code;

    auto symbol = reader::read_string(data);
    if (!symbol) return std::unexpected(symbol.error());
    version.data.symbol = *symbol;

    auto fraction_symbol = reader::read_string(data);
    if (!fraction_symbol) return std::unexpected(fraction_symbol.error());
    version.data.fraction_symbol = *fraction_symbol;

    auto fractions_per_unit = reader::read_uint32(data);
    if (!fractions_per_unit) return std::unexpected(fractions_per_unit.error());
    version.data.fractions_per_unit = static_cast<int>(*fractions_per_unit);

    auto rounding_type = reader::read_string(data);
    if (!rounding_type) return std::unexpected(rounding_type.error());
    version.data.rounding_type = *rounding_type;

    auto rounding_precision = reader::read_uint32(data);
    if (!rounding_precision) return std::unexpected(rounding_precision.error());
    version.data.rounding_precision = static_cast<int>(*rounding_precision);

    auto format = reader::read_string(data);
    if (!format) return std::unexpected(format.error());
    version.data.format = *format;

    auto monetary_nature = reader::read_string(data);
    if (!monetary_nature) return std::unexpected(monetary_nature.error());
    version.data.monetary_nature = *monetary_nature;

    auto market_tier = reader::read_string(data);
    if (!market_tier) return std::unexpected(market_tier.error());
    version.data.market_tier = *market_tier;

    auto recorded_at = reader::read_string(data);
    if (!recorded_at) return std::unexpected(recorded_at.error());
    try {
        version.data.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    // Read optional image_id: bool flag followed by UUID if present
    auto has_image_id = reader::read_bool(data);
    if (!has_image_id) return std::unexpected(has_image_id.error());
    if (*has_image_id) {
        auto image_id = reader::read_uuid(data);
        if (!image_id) return std::unexpected(image_id.error());
        version.data.image_id = *image_id;
    }

    // Read version metadata
    auto version_number = reader::read_uint32(data);
    if (!version_number) return std::unexpected(version_number.error());
    version.version_number = static_cast<int>(*version_number);


    auto version_recorded_at = reader::read_string(data);
    if (!version_recorded_at) return std::unexpected(version_recorded_at.error());
    try {
        version.recorded_at = ores::platform::time::datetime::parse_time_point(*version_recorded_at);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto change_summary = reader::read_string(data);
    if (!change_summary) return std::unexpected(change_summary.error());
    version.change_summary = *change_summary;

    return version;
}

}

namespace ores::refdata::messaging {

std::vector<std::byte> get_currency_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, iso_code);
    return buffer;
}

std::expected<get_currency_history_request, ores::utility::serialization::error_code>
get_currency_history_request::deserialize(std::span<const std::byte> data) {
    auto iso_code_result = reader::read_string(data);
    if (!iso_code_result) {
        return std::unexpected(iso_code_result.error());
    }
    return get_currency_history_request{*iso_code_result};
}

std::ostream& operator<<(std::ostream& s, const get_currency_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_currency_history_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write success flag and message
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);

    // Write ISO code
    writer::write_string(buffer, history.iso_code);

    // Write version count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(history.versions.size()));

    // Write each version
    for (const auto& version : history.versions) {
        serialize_currency_version(buffer, version);
    }

    return buffer;
}

std::expected<get_currency_history_response, ores::utility::serialization::error_code>
get_currency_history_response::deserialize(std::span<const std::byte> data) {
    get_currency_history_response response;

    // Read success flag and message
    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    // If request failed, don't try to read history data
    if (!response.success) {
        return response;
    }

    // Read ISO code
    auto iso_code_result = reader::read_string(data);
    if (!iso_code_result) return std::unexpected(iso_code_result.error());
    response.history.iso_code = *iso_code_result;

    // Read version count
    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    // Read each version
    response.history.versions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto version_result = deserialize_currency_version(data);
        if (!version_result) return std::unexpected(version_result.error());
        response.history.versions.push_back(std::move(*version_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_currency_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
