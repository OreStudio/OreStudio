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
#include "ores.utility/faker/datetime.hpp"

#include <chrono>
#include <format>
#include <random>

namespace ores::utility::faker {

std::chrono::system_clock::time_point datetime::past_timepoint() {
    using namespace std::chrono;
    static thread_local std::mt19937_64 rng{std::random_device{}()};

    const auto min_time = sys_days{year{1970}/1/1}.time_since_epoch();
    const auto max_time = sys_days{year{2039}/1/1}.time_since_epoch() - 1s;

    std::uniform_int_distribution<std::int64_t> dist(
        min_time.count(),
        max_time.count()
    );

    return sys_seconds{seconds{dist(rng)}};
}

std::string datetime::past_string() {
    auto tp = past_timepoint();
    return std::format("{:%Y-%m-%d %H:%M:%S}", tp);
}

std::chrono::system_clock::time_point datetime::make_timepoint(
    int y, int m, int d, int hour, int min, int sec) {
    using namespace std::chrono;
    return sys_days{year_month_day{year{y}, month{static_cast<unsigned>(m)},
        day{static_cast<unsigned>(d)}}} + hours{hour} + minutes{min} +
        seconds{sec};
}

}
