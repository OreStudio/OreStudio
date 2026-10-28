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
#include "ores.marketdata.core/repository/fixing_read_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.marketdata.core/repository/pg_array.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <cstdint>
#include <format>
#include <stdexcept>
#include <string_view>

namespace ores::marketdata::repository {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

namespace {

std::string to_text(std::chrono::year_month_day d) {
    return std::format("{:04}-{:02}-{:02}",
                       static_cast<int>(d.year()),
                       static_cast<unsigned>(d.month()),
                       static_cast<unsigned>(d.day()));
}

int number(std::string_view text) {
    int out = 0;
    const auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), out);
    if (ec != std::errc{} || end != text.data() + text.size())
        throw std::runtime_error("Not a number in a stored date: " + std::string(text));
    return out;
}

std::chrono::year_month_day date_of(const std::string& text) {
    if (text.size() != 10 || text[4] != '-' || text[7] != '-')
        throw std::runtime_error("Not a stored date: " + text);
    const std::chrono::year_month_day d{
        std::chrono::year{number(std::string_view(text).substr(0, 4))},
        std::chrono::month{static_cast<unsigned>(number(std::string_view(text).substr(5, 2)))},
        std::chrono::day{static_cast<unsigned>(number(std::string_view(text).substr(8, 2)))}};
    if (!d.ok())
        throw std::runtime_error("Not a stored date: " + text);
    return d;
}

std::chrono::system_clock::time_point instant_of(const std::string& micros) {
    std::int64_t us = 0;
    const auto [end, ec] = std::from_chars(micros.data(), micros.data() + micros.size(), us);
    if (ec != std::errc{} || end != micros.data() + micros.size())
        throw std::runtime_error("Not a stored instant: " + micros);
    return std::chrono::system_clock::time_point(
        std::chrono::duration_cast<std::chrono::system_clock::duration>(
            std::chrono::microseconds(us)));
}

// The date as ISO text, the value as stored, the source, and the time stored as whole
// microseconds since the epoch, which parse back without a rounding step.
constexpr std::string_view fixing_columns =
    "f.fixing_date::text, f.value, coalesce(f.source, ''), "
    "(extract(epoch FROM f.valid_from) * 1000000)::bigint::text";

std::vector<stored_fixing>
to_fixings(const std::vector<std::vector<std::optional<std::string>>>& rows) {
    std::vector<stored_fixing> out;
    out.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 4 || !row[0] || !row[1] || !row[3])
            throw std::runtime_error("A stored fixing came back without its date, value or time");
        out.push_back({.date = date_of(*row[0]),
                       .value = *row[1],
                       .source = row[2].value_or(""),
                       .received_at = instant_of(*row[3])});
    }
    return out;
}

}

std::vector<fixing_series> fixing_read_repository::series_of(context ctx,
                                                             const std::string& item,
                                                             const std::string& provider_code) {
    std::vector<std::string> parties;
    for (const auto& p : ctx.visible_party_ids())
        parties.push_back(boost::uuids::to_string(p));
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        "SELECT id::text, party_id::text, provider_code FROM ores_marketdata_market_series_tbl "
        "WHERE oresmd_uri = $1 AND party_id = ANY($2::uuid[]) "
        "AND ($3 = '' OR provider_code = $3) "
        "AND valid_to = ores_utility_infinity_timestamp_fn()",
        {item, pg_array(parties), provider_code},
        lg(),
        "reading the series of a fixing item");
    std::vector<fixing_series> out;
    boost::uuids::string_generator gen;
    for (const auto& row : rows) {
        if (row.size() < 3 || !row[0] || !row[1])
            continue;
        out.push_back(
            {.id = gen(*row[0]), .party_id = gen(*row[1]), .provider_code = row[2].value_or("")});
    }
    return out;
}

std::vector<stored_fixing> fixing_read_repository::on_or_before(context ctx,
                                                                const boost::uuids::uuid& series_id,
                                                                std::chrono::year_month_day date,
                                                                std::size_t limit) {
    return to_fixings(execute_parameterized_multi_column_query(
        ctx,
        std::format("SELECT {} FROM ores_marketdata_market_fixings_tbl f "
                    "WHERE f.series_id = $1::uuid AND f.fixing_date <= $2::date "
                    "AND f.valid_to = ores_utility_infinity_timestamp_fn() "
                    "ORDER BY f.fixing_date DESC LIMIT $3::int",
                    fixing_columns),
        {boost::uuids::to_string(series_id), to_text(date), std::to_string(limit)},
        lg(),
        "reading the fixings of a series on or before a date"));
}

std::vector<stored_fixing> fixing_read_repository::between(context ctx,
                                                           const boost::uuids::uuid& series_id,
                                                           std::chrono::year_month_day from,
                                                           std::chrono::year_month_day to,
                                                           std::size_t limit) {
    return to_fixings(execute_parameterized_multi_column_query(
        ctx,
        std::format("SELECT {} FROM ores_marketdata_market_fixings_tbl f "
                    "WHERE f.series_id = $1::uuid AND f.fixing_date BETWEEN $2::date AND $3::date "
                    "AND f.valid_to = ores_utility_infinity_timestamp_fn() "
                    "ORDER BY f.fixing_date LIMIT $4::int",
                    fixing_columns),
        {boost::uuids::to_string(series_id), to_text(from), to_text(to), std::to_string(limit)},
        lg(),
        "reading the fixings of a series in a window"));
}

std::optional<vendor_key> fixing_read_repository::vendor_of(context ctx,
                                                            const std::string& item,
                                                            const std::string& provider_code) {
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        "SELECT vi.id_scheme, vi.external_id "
        "FROM ores_marketdata_market_data_mappings_tbl m "
        "JOIN ores_marketdata_market_data_vendor_instruments_tbl vi "
        "  ON vi.id = m.vendor_instrument_id "
        " AND vi.valid_to = ores_utility_infinity_timestamp_fn() "
        "WHERE m.oresmd_uri = $1 AND vi.provider_code = $2 "
        "AND m.valid_to = ores_utility_infinity_timestamp_fn() "
        "ORDER BY vi.id_scheme, vi.external_id LIMIT 1",
        {item, provider_code},
        lg(),
        "reading the vendor id of a fixing item");
    if (rows.empty() || rows.front().size() < 2 || !rows.front()[1])
        return std::nullopt;
    return vendor_key{.scheme = rows.front()[0].value_or(""), .id = *rows.front()[1]};
}

}
