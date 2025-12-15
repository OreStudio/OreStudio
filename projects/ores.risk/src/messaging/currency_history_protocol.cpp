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
#include "ores.risk/messaging/currency_history_protocol.hpp"

#include <expected>
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

using namespace ores::risk;
using namespace ores::comms::messaging;

namespace {

/**
 * @brief Helper function to serialize a single currency version
 */
void serialize_currency_version(std::vector<std::byte>& buffer, const domain::currency_version& version) {
    // Write currency data
    writer::write_string(buffer, version.data.iso_code);
    writer::write_string(buffer, version.data.name);
    writer::write_string(buffer, version.data.numeric_code);
    writer::write_string(buffer, version.data.symbol);
    writer::write_string(buffer, version.data.fraction_symbol);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.data.fractions_per_unit));
    writer::write_string(buffer, version.data.rounding_type);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.data.rounding_precision));
    writer::write_string(buffer, version.data.format);
    writer::write_string(buffer, version.data.currency_type);
    writer::write_string(buffer, version.data.modified_by);
    writer::write_string(buffer, version.data.recorded_at);

    // Write version metadata
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.version_number));
    writer::write_string(buffer, version.modified_by);
    writer::write_string(buffer, version.modified_at);
    writer::write_string(buffer, version.change_summary);
}

std::expected<domain::currency_version, error_code>
deserialize_currency_version(std::span<const std::byte>& data) {
    domain::currency_version version;

    // Read currency data
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

    auto currency_type = reader::read_string(data);
    if (!currency_type) return std::unexpected(currency_type.error());
    version.data.currency_type = *currency_type;

    auto modified_by = reader::read_string(data);
    if (!modified_by) return std::unexpected(modified_by.error());
    version.data.modified_by = *modified_by;

    auto recorded_at = reader::read_string(data);
    if (!recorded_at) return std::unexpected(recorded_at.error());
    version.data.recorded_at = *recorded_at;

    // Read version metadata
    auto version_number = reader::read_uint32(data);
    if (!version_number) return std::unexpected(version_number.error());
    version.version_number = static_cast<int>(*version_number);

    auto version_modified_by = reader::read_string(data);
    if (!version_modified_by) return std::unexpected(version_modified_by.error());
    version.modified_by = *version_modified_by;

    auto modified_at = reader::read_string(data);
    if (!modified_at) return std::unexpected(modified_at.error());
    version.modified_at = *modified_at;

    auto change_summary = reader::read_string(data);
    if (!change_summary) return std::unexpected(change_summary.error());
    version.change_summary = *change_summary;

    return version;
}

}

namespace ores::risk::messaging {

std::vector<std::byte> get_currency_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, iso_code);
    return buffer;
}

std::expected<get_currency_history_request, comms::messaging::error_code>
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

std::expected<get_currency_history_response, comms::messaging::error_code>
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
    auto count_result = reader::read_uint32(data);
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
