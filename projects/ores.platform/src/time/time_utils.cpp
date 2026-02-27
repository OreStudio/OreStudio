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
#include "ores.platform/time/time_utils.hpp"

namespace ores::platform::time {

std::tm* time_utils::gmtime_safe(const std::time_t* time, std::tm* result) {
#ifdef _WIN32
    // Windows: gmtime_s has reversed parameter order and returns errno_t
    if (gmtime_s(result, time) == 0) {
        return result;
    }
    return nullptr;
#else
    // POSIX: gmtime_r returns pointer to result
    return gmtime_r(time, result);
#endif
}

std::tm* time_utils::localtime_safe(const std::time_t* time, std::tm* result) {
#ifdef _WIN32
    // Windows: localtime_s has reversed parameter order and returns errno_t
    if (localtime_s(result, time) == 0) {
        return result;
    }
    return nullptr;
#else
    // POSIX: localtime_r returns pointer to result
    return localtime_r(time, result);
#endif
}

std::time_t time_utils::timegm_safe(std::tm* tm) {
#ifdef _WIN32
    return _mkgmtime(tm);
#else
    return timegm(tm);
#endif
}

}
