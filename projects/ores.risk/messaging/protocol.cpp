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
#include "ores.risk/messaging/protocol.hpp"

#include <expected>
#include "ores.utility/messaging/write.hpp"

namespace ores::risk::messaging {

using namespace ores::utility::messaging;

// get_currencies_request implementation
std::vector<std::uint8_t> get_currencies_request::serialize() const {
    // Empty payload - no parameters
    return {};
}

std::expected<get_currencies_request, comms::protocol::error_code>
get_currencies_request::deserialize(std::span<const std::uint8_t> data) {
    // Empty payload expected
    if (!data.empty()) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    return get_currencies_request{};
}

std::ostream& operator<<(std::ostream& s, const get_currencies_request& v)
{
    rfl::json::write(v, s);
    return(s);
}

std::vector<std::uint8_t> get_currencies_response::serialize() const {
    std::vector<std::uint8_t> buffer;

    // Write currency count
    write_uint32(buffer, static_cast<std::uint32_t>(currencies.size()));

    // Write each currency
    for (const auto& currency : currencies) {
        write_string(buffer, currency.iso_code);
        write_string(buffer, currency.name);
        write_string(buffer, currency.numeric_code);
        write_string(buffer, currency.symbol);
        write_string(buffer, currency.fraction_symbol);
        write_uint32(buffer, static_cast<std::uint32_t>(currency.fractions_per_unit));
        write_string(buffer, currency.rounding_type);
        write_uint32(buffer, static_cast<std::uint32_t>(currency.rounding_precision));
        write_string(buffer, currency.format);
        write_string(buffer, currency.currency_type);
        write_string(buffer, currency.modified_by);
        write_string(buffer, currency.valid_from);
        write_string(buffer, currency.valid_to);
    }

    return buffer;
}

std::expected<get_currencies_response, comms::protocol::error_code>
get_currencies_response::deserialize(std::span<const std::uint8_t> data) {
    get_currencies_response response;

    // Read currency count
    auto count_result = read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    // Read each currency
    response.currencies.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::currency currency;

        auto iso_code = read_string(data);
        if (!iso_code) return std::unexpected(iso_code.error());
        currency.iso_code = *iso_code;

        auto name = read_string(data);
        if (!name) return std::unexpected(name.error());
        currency.name = *name;

        auto numeric_code = read_string(data);
        if (!numeric_code) return std::unexpected(numeric_code.error());
        currency.numeric_code = *numeric_code;

        auto symbol = read_string(data);
        if (!symbol) return std::unexpected(symbol.error());
        currency.symbol = *symbol;

        auto fraction_symbol = read_string(data);
        if (!fraction_symbol) return std::unexpected(fraction_symbol.error());
        currency.fraction_symbol = *fraction_symbol;

        auto fractions_per_unit = read_uint32(data);
        if (!fractions_per_unit) return std::unexpected(fractions_per_unit.error());
        currency.fractions_per_unit = static_cast<int>(*fractions_per_unit);

        auto rounding_type = read_string(data);
        if (!rounding_type) return std::unexpected(rounding_type.error());
        currency.rounding_type = *rounding_type;

        auto rounding_precision = read_uint32(data);
        if (!rounding_precision) return std::unexpected(rounding_precision.error());
        currency.rounding_precision = static_cast<int>(*rounding_precision);

        auto format = read_string(data);
        if (!format) return std::unexpected(format.error());
        currency.format = *format;

        auto currency_type = read_string(data);
        if (!currency_type) return std::unexpected(currency_type.error());
        currency.currency_type = *currency_type;

        auto modified_by = read_string(data);
        if (!modified_by) return std::unexpected(modified_by.error());
        currency.modified_by = *modified_by;

        auto valid_from = read_string(data);
        if (!valid_from) return std::unexpected(valid_from.error());
        currency.valid_from = *valid_from;

        auto valid_to = read_string(data);
        if (!valid_to) return std::unexpected(valid_to.error());
        currency.valid_to = *valid_to;

        response.currencies.push_back(std::move(currency));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_currencies_response& v)
{
    rfl::json::write(v, s);
    return(s);
}


}
