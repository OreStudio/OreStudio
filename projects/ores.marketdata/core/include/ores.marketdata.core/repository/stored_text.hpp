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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_STORED_TEXT_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_STORED_TEXT_HPP

#include <charconv>
#include <chrono>
#include <cstdint>
#include <format>
#include <stdexcept>
#include <string>

namespace ores::marketdata::repository {

/// A date as the ISO text a read passes as a parameter.
[[nodiscard]] inline std::string date_text(std::chrono::year_month_day d) {
    return std::format("{:04}-{:02}-{:02}",
                       static_cast<int>(d.year()),
                       static_cast<unsigned>(d.month()),
                       static_cast<unsigned>(d.day()));
}

/**
 * @brief An instant read back as whole microseconds since the epoch.
 *
 * A read selects a timestamp as a bigint of microseconds, which parses back without a
 * rounding step.
 */
[[nodiscard]] inline std::chrono::system_clock::time_point
instant_of_micros(const std::string& micros) {
    std::int64_t us = 0;
    const auto [end, ec] = std::from_chars(micros.data(), micros.data() + micros.size(), us);
    if (ec != std::errc{} || end != micros.data() + micros.size())
        throw std::runtime_error("Not a stored instant: " + micros);
    return std::chrono::system_clock::time_point(
        std::chrono::duration_cast<std::chrono::system_clock::duration>(
            std::chrono::microseconds(us)));
}

}

#endif
