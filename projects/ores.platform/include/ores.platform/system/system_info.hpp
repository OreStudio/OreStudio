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
#ifndef ORES_PLATFORM_SYSTEM_SYSTEM_INFO_HPP
#define ORES_PLATFORM_SYSTEM_SYSTEM_INFO_HPP

#include "ores.platform/export.hpp"
#include <cstdint>

namespace ores::platform::system {

/**
 * @brief What a computer says about its hardware.
 *
 * A figure the computer does not give stays at zero, so a caller shows nothing
 * for it and never a made-up value.
 */
struct system_info {
    /**
     * @brief Logical processors, counting the hardware threads.
     */
    int cpu_count = 0;
    /**
     * @brief Physical memory in MiB.
     */
    std::int64_t ram_mb = 0;
};

/**
 * @brief Reads the hardware facts of the computer the process runs on.
 *
 * Inside a container the figures are the host's, not the container's limits,
 * because a container with no limit set sees the whole computer.
 *
 * @return The facts the computer gave.
 */
ORES_PLATFORM_EXPORT system_info get_system_info();

}

#endif
