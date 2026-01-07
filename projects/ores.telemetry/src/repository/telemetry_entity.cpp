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
#include "ores.telemetry/repository/telemetry_entity.hpp"

#include <ostream>

namespace ores::telemetry::repository {

std::ostream& operator<<(std::ostream& s, const telemetry_entity& v) {
    s << "telemetry_entity["
      << "id=" << v.id.value()
      << ", timestamp=" << v.timestamp.value().str()
      << ", source=" << v.source
      << ", source_name=" << v.source_name
      << ", level=" << v.level
      << ", component=" << v.component
      << ", message=" << (v.message.size() > 50
            ? v.message.substr(0, 50) + "..."
            : v.message)
      << "]";
    return s;
}

std::ostream& operator<<(std::ostream& s, const telemetry_stats_hourly_entity& v) {
    s << "telemetry_stats_hourly_entity["
      << "hour=" << v.hour
      << ", source=" << v.source
      << ", source_name=" << v.source_name
      << ", level=" << v.level
      << ", log_count=" << v.log_count
      << ", unique_sessions=" << v.unique_sessions
      << ", unique_accounts=" << v.unique_accounts
      << "]";
    return s;
}

std::ostream& operator<<(std::ostream& s, const telemetry_stats_daily_entity& v) {
    s << "telemetry_stats_daily_entity["
      << "day=" << v.day
      << ", source=" << v.source
      << ", source_name=" << v.source_name
      << ", component=" << v.component
      << ", level=" << v.level
      << ", log_count=" << v.log_count
      << ", unique_sessions=" << v.unique_sessions
      << ", unique_accounts=" << v.unique_accounts
      << "]";
    return s;
}

}
