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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.core/repository/observation_read_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <cstdint>
#include <format>
#include <optional>
#include <stdexcept>
#include <string>

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

std::chrono::system_clock::time_point instant_of(const std::string& micros) {
    std::int64_t us = 0;
    const auto [end, ec] = std::from_chars(micros.data(), micros.data() + micros.size(), us);
    if (ec != std::errc{} || end != micros.data() + micros.size())
        throw std::runtime_error("Not a stored instant: " + micros);
    return std::chrono::system_clock::time_point(
        std::chrono::duration_cast<std::chrono::system_clock::duration>(
            std::chrono::microseconds(us)));
}

}

std::vector<stored_observation>
observation_read_repository::between(context ctx,
                                     const boost::uuids::uuid& series_id,
                                     std::chrono::year_month_day from,
                                     std::chrono::year_month_day to,
                                     std::size_t limit) {
    const auto after =
        std::chrono::year_month_day{std::chrono::sys_days{to} + std::chrono::days{1}};
    // The time as whole microseconds since the epoch, which parse back without a rounding
    // step; the window is half open in UTC so the last day is read to its end.
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        "SELECT (extract(epoch FROM o.observation_datetime) * 1000000)::bigint::text, "
        "o.oresmd_uri, o.value, coalesce(o.source, '') "
        "FROM ores_marketdata_market_observations_tbl o "
        "WHERE o.series_id = $1::uuid "
        "AND o.observation_datetime >= ($2::date)::timestamp AT TIME ZONE 'UTC' "
        "AND o.observation_datetime < ($3::date)::timestamp AT TIME ZONE 'UTC' "
        "AND o.valid_to = ores_utility_infinity_timestamp_fn() "
        "ORDER BY o.observation_datetime, o.oresmd_uri LIMIT $4::int",
        {boost::uuids::to_string(series_id), to_text(from), to_text(after), std::to_string(limit)},
        lg(),
        "reading the observations of a series in a window");
    std::vector<stored_observation> out;
    out.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 4 || !row[0] || !row[1] || !row[2])
            throw std::runtime_error(
                "A stored observation came back without its time, coordinate or value");
        out.push_back({.observed_at = instant_of(*row[0]),
                       .coordinate = *row[1],
                       .value = *row[2],
                       .source = row[3].value_or("")});
    }
    return out;
}

}
