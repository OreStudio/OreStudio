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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_PLATFORM_TIME_ZONE_OFFSET_HPP
#define ORES_PLATFORM_TIME_ZONE_OFFSET_HPP

#include "ores.platform/export.hpp"
#include <chrono>
#include <optional>
#include <string_view>

namespace ores::platform::time {

/**
 * @brief The offset an IANA zone keeps from UTC at an instant.
 *
 * The offset is read from the system's time zone database, which a POSIX system
 * keeps under @c /usr/share/zoneinfo in the TZif format. A reader is needed
 * because the C++ library is not required to carry std::chrono's time zone
 * database: libc++ does not, so on macOS a call to std::chrono::locate_zone
 * does not compile even with <chrono> included.
 *
 * The reading is thread safe and changes no process state, so it may be called
 * from a service's own thread beside anything else the process is doing.
 *
 * @param iana_zone the zone, such as "America/New_York".
 * @param instant the moment to read the offset at.
 *
 * @returns the offset, or nothing when the zone is unknown to the database, the
 *          database cannot be read, or the file does not describe a transition
 *          that covers @p instant.
 *
 * @note An instant later than the last transition a file lists is answered with
 *       that transition's type. Zone files list transitions decades ahead, so
 *       this matters only to dates no clock in this system reaches.
 */
ORES_PLATFORM_EXPORT std::optional<std::chrono::seconds> zone_offset_at(
    std::string_view iana_zone,
    std::chrono::system_clock::time_point instant);

}

#endif
