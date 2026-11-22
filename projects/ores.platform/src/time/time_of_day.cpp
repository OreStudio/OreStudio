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
#include "ores.platform/time/time_of_day.hpp"
#include <format>
#include <stdexcept>

namespace ores::platform::time {

time_of_day::time_of_day(std::chrono::seconds since_midnight)
    : since_midnight_(since_midnight) {
    if (since_midnight < std::chrono::seconds{0} || since_midnight >= std::chrono::days{1})
        throw std::out_of_range(
            std::format("Not a time of day: {}s after midnight", since_midnight.count()));
}

time_of_day::time_of_day(std::chrono::hours h, std::chrono::minutes m, std::chrono::seconds s)
    : time_of_day(h + m + s) {}

}
