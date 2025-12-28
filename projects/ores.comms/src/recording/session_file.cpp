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
#include "ores.comms/recording/session_file.hpp"

#include <sstream>
#include <boost/uuid/uuid_io.hpp>
#include "ores.platform/time/datetime.hpp"

namespace ores::comms::recording {

std::string generate_session_filename(
    const boost::uuids::uuid& session_id,
    std::chrono::system_clock::time_point start_time) {

    // Format timestamp as YYYYMMDD-HHMMSS using cross-platform utility
    auto timestamp_str = ores::platform::time::datetime::format_time_point(
        start_time, "%Y%m%d-%H%M%S");

    std::ostringstream oss;
    oss << "session-" << timestamp_str << "-";

    // Use first 8 characters of UUID (short form)
    std::ostringstream uuid_oss;
    uuid_oss << session_id;
    const auto uuid_str = uuid_oss.str();
    oss << uuid_str.substr(0, 8);

    oss << ".ores";

    return oss.str();
}

}
