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
#include "ores.refdata/messaging/currency_protocol.hpp"

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
 * @brief Helper function to serialize a single currency
 */
void serialize_currency(std::vector<std::byte>& buffer, const domain::currency& currency) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(currency.version));
    writer::write_string(buffer, currency.iso_code);
    writer::write_string(buffer, currency.name);
    writer::write_string(buffer, currency.numeric_code);
    writer::write_string(buffer, currency.symbol);
    writer::write_string(buffer, currency.fraction_symbol);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(currency.fractions_per_unit));
    writer::write_string(buffer, currency.rounding_type);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(currency.rounding_precision));
    writer::write_string(buffer, currency.format);
    writer::write_string(buffer, currency.currency_type);
    // Write optional image_id: bool flag followed by UUID if present
    writer::write_bool(buffer, currency.image_id.has_value());
    if (currency.image_id) {
        writer::write_uuid(buffer, *currency.image_id);
    }
    writer::write_string(buffer, currency.change_reason_code);
    writer::write_string(buffer, currency.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(currency.recorded_at));
}

std::expected<domain::currency, ores::utility::serialization::error_code>
deserialize_currency(std::span<const std::byte>& data) {
    domain::currency currency;

    auto version = reader::read_uint32(data);
    if (!version) return std::unexpected(version.error());
    currency.version = static_cast<int>(*version);

    auto iso_code = reader::read_string(data);
    if (!iso_code) return std::unexpected(iso_code.error());
    currency.iso_code = *iso_code;

    auto name = reader::read_string(data);
    if (!name) return std::unexpected(name.error());
    currency.name = *name;

    auto numeric_code = reader::read_string(data);
    if (!numeric_code) return std::unexpected(numeric_code.error());
    currency.numeric_code = *numeric_code;

    auto symbol = reader::read_string(data);
    if (!symbol) return std::unexpected(symbol.error());
    currency.symbol = *symbol;

    auto fraction_symbol = reader::read_string(data);
    if (!fraction_symbol) return std::unexpected(fraction_symbol.error());
    currency.fraction_symbol = *fraction_symbol;

    auto fractions_per_unit = reader::read_uint32(data);
    if (!fractions_per_unit) return std::unexpected(fractions_per_unit.error());
    currency.fractions_per_unit = static_cast<int>(*fractions_per_unit);

    auto rounding_type = reader::read_string(data);
    if (!rounding_type) return std::unexpected(rounding_type.error());
    currency.rounding_type = *rounding_type;

    auto rounding_precision = reader::read_uint32(data);
    if (!rounding_precision) return std::unexpected(rounding_precision.error());
    currency.rounding_precision = static_cast<int>(*rounding_precision);

    auto format = reader::read_string(data);
    if (!format) return std::unexpected(format.error());
    currency.format = *format;

    auto currency_type = reader::read_string(data);
    if (!currency_type) return std::unexpected(currency_type.error());
    currency.currency_type = *currency_type;

    // Read optional image_id: bool flag followed by UUID if present
    auto has_image_id = reader::read_bool(data);
    if (!has_image_id) return std::unexpected(has_image_id.error());
    if (*has_image_id) {
        auto image_id = reader::read_uuid(data);
        if (!image_id) return std::unexpected(image_id.error());
        currency.image_id = *image_id;
    }


    auto change_reason_code = reader::read_string(data);
    if (!change_reason_code) return std::unexpected(change_reason_code.error());
    currency.change_reason_code = *change_reason_code;

    auto change_commentary = reader::read_string(data);
    if (!change_commentary) return std::unexpected(change_commentary.error());
    currency.change_commentary = *change_commentary;

    auto recorded_at = reader::read_string(data);
    if (!recorded_at) return std::unexpected(recorded_at.error());
    try {
        currency.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return currency;
}

}

namespace ores::refdata::messaging {

std::vector<std::byte> delete_currency_request::serialize() const {
    std::vector<std::byte> buffer;

    // Write count of ISO codes
    writer::write_uint32(buffer, static_cast<std::uint32_t>(iso_codes.size()));

    // Write each ISO code
    for (const auto& iso_code : iso_codes) {
        writer::write_string(buffer, iso_code);
    }

    return buffer;
}

std::expected<delete_currency_request, ores::utility::serialization::error_code>
delete_currency_request::deserialize(std::span<const std::byte> data) {
    delete_currency_request request;

    // Read count
    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    // Read each ISO code
    request.iso_codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto iso_code_result = reader::read_string(data);
        if (!iso_code_result) {
            return std::unexpected(iso_code_result.error());
        }
        request.iso_codes.push_back(*iso_code_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_currency_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_currency_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_currency_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write count of results
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));

    // Write each result
    for (const auto& result : results) {
        writer::write_string(buffer, result.iso_code);
        writer::write_bool(buffer, result.success);
        writer::write_string(buffer, result.message);
    }

    return buffer;
}

std::expected<delete_currency_response, ores::utility::serialization::error_code>
delete_currency_response::deserialize(std::span<const std::byte> data) {
    delete_currency_response response;

    // Read count
    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    const auto count = *count_result;

    // Read each result
    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_currency_result result;

        auto iso_code_result = reader::read_string(data);
        if (!iso_code_result) {
            return std::unexpected(iso_code_result.error());
        }
        result.iso_code = *iso_code_result;

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

std::ostream& operator<<(std::ostream& s, const delete_currency_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_currencies_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_currencies_request, ores::utility::serialization::error_code>
get_currencies_request::deserialize(std::span<const std::byte> data) {
    get_currencies_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_currencies_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_currencies_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write currency count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(currencies.size()));

    // Write each currency using the serialize helper
    for (const auto& currency : currencies) {
        serialize_currency(buffer, currency);
    }

    return buffer;
}

std::expected<get_currencies_response, ores::utility::serialization::error_code>
get_currencies_response::deserialize(std::span<const std::byte> data) {
    get_currencies_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) {
        return std::unexpected(total_result.error());
    }
    response.total_available_count = *total_result;

    // Read currency count in this response
    auto count_result = reader::read_count(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    // Read each currency using the deserialize helper
    response.currencies.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto currency_result = deserialize_currency(data);
        if (!currency_result) {
            return std::unexpected(currency_result.error());
        }
        response.currencies.push_back(std::move(*currency_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_currencies_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_currency_request::serialize() const {
    std::vector<std::byte> buffer;
    serialize_currency(buffer, currency);
    return buffer;
}

std::expected<save_currency_request, ores::utility::serialization::error_code>
save_currency_request::deserialize(std::span<const std::byte> data) {
    auto currency_result = deserialize_currency(data);
    if (!currency_result) {
        return std::unexpected(currency_result.error());
    }
    return save_currency_request{*currency_result};
}

std::ostream& operator<<(std::ostream& s, const save_currency_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_currency_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_currency_response, ores::utility::serialization::error_code>
save_currency_response::deserialize(std::span<const std::byte> data) {
    save_currency_response response;

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

std::ostream& operator<<(std::ostream& s, const save_currency_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
