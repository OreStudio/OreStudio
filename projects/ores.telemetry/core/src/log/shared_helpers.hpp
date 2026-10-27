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
#ifndef ORES_TELEMETRY_CORE_SRC_LOG_SHARED_HELPERS_HPP_
#define ORES_TELEMETRY_CORE_SRC_LOG_SHARED_HELPERS_HPP_

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::telemetry::log {

inline std::chrono::system_clock::time_point to_system_clock(const boost::posix_time::ptime& pt) {
    if (pt.is_not_a_date_time()) {
        return std::chrono::system_clock::now();
    }

    static const boost::posix_time::ptime epoch(boost::gregorian::date(1970, 1, 1));
    const auto duration = pt - epoch;
    const auto microseconds = duration.total_microseconds();
    return std::chrono::system_clock::time_point(std::chrono::microseconds(microseconds));
}

}

#endif
