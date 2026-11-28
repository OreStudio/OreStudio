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
#include <optional>
#include <string>

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
 * @brief The space of one filesystem, as the computer states it.
 *
 * Both figures are bytes. The free figure is what a write may still use without
 * touching the reserve the root account keeps, because that is the space a
 * database can grow into; counting the reserve would report room the server
 * cannot take.
 */
struct filesystem_space {
    std::uint64_t free_bytes = 0;
    std::uint64_t total_bytes = 0;
};

/**
 * @brief Reads the space of the filesystem that holds a path.
 *
 * A path this computer does not hold, or one whose filesystem the process may not
 * ask about, answers nothing: a zero would read as a full disk, which is a
 * different fact from a disk that could not be measured.
 */
ORES_PLATFORM_EXPORT std::optional<filesystem_space> get_filesystem_space(const std::string& path);

/**
 * @brief The name the computer gives itself, or a stated placeholder when it
 * gives none.
 */
ORES_PLATFORM_EXPORT std::string get_host_name();

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
