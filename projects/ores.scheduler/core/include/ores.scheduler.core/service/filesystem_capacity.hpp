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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SCHEDULER_CORE_SERVICE_FILESYSTEM_CAPACITY_HPP
#define ORES_SCHEDULER_CORE_SERVICE_FILESYSTEM_CAPACITY_HPP

#include "ores.platform/system/system_info.hpp"
#include "ores.scheduler.core/export.hpp"
#include <optional>
#include <string>

namespace ores::scheduler::service {

/**
 * @brief The space of one filesystem, as the host states it.
 *
 * Both figures are bytes. The free figure is what a write may still use without
 * touching the reserve the root account keeps, because that is the space a
 * database can grow into; counting the reserve would report room the server
 * cannot take.
 */
struct filesystem_capacity {
    std::uint64_t free_bytes = 0;
    std::uint64_t total_bytes = 0;
};

/**
 * @brief Measures the filesystem that holds a path, or nothing when it cannot.
 *
 * A path that does not exist on this host, or one whose filesystem the process
 * may not ask about, answers nothing and says why. The caller records the
 * database's size either way: an unknown free space is a fact worth keeping, and
 * a zero would state a full disk.
 *
 * The function is free so a test can measure a temporary directory without a
 * database or a service around it.
 */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT std::optional<filesystem_capacity>
filesystem_capacity_of(const std::string& path, std::string& error);

/** @brief This process's host name, or a stated placeholder when it has none. */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT std::string this_host_name();

}

#endif
