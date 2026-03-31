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
#include "ores.ore/market/market_data_serializer.hpp"

#include <format>
#include <ostream>

namespace ores::ore::market {

namespace {

std::string format_yyyymmdd(std::chrono::year_month_day ymd) {
    return std::format("{:04d}{:02d}{:02d}",
        static_cast<int>(ymd.year()),
        static_cast<unsigned>(ymd.month()),
        static_cast<unsigned>(ymd.day()));
}

std::string format_yyyy_mm_dd(std::chrono::year_month_day ymd) {
    return std::format("{:04d}-{:02d}-{:02d}",
        static_cast<int>(ymd.year()),
        static_cast<unsigned>(ymd.month()),
        static_cast<unsigned>(ymd.day()));
}

} // namespace

void serialize_market_data(std::ostream& out,
                           const std::vector<market_datum>& data) {
    for (const auto& d : data)
        out << format_yyyymmdd(d.date) << '\t' << d.key << '\t' << d.value << '\n';
}

void serialize_fixings(std::ostream& out,
                       const std::vector<fixing>& fixings) {
    for (const auto& f : fixings)
        out << format_yyyy_mm_dd(f.date) << '\t' << f.index_name << '\t' << f.value << '\n';
}

} // namespace ores::ore::market
