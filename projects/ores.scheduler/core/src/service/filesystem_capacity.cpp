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
#include "ores.scheduler.core/service/filesystem_capacity.hpp"

namespace ores::scheduler::service {

std::optional<filesystem_capacity> filesystem_capacity_of(const std::string& path,
                                                          std::string& error) {
    // The measurement is the platform's: asking a filesystem how much room it has
    // is one call per operating system, and no scheduler should hold that.
    const auto space = ores::platform::system::get_filesystem_space(path);
    if (!space) {
        error = "the filesystem at '" + path + "' could not be measured";
        return std::nullopt;
    }
    error.clear();
    return filesystem_capacity{space->free_bytes, space->total_bytes};
}

std::string this_host_name() {
    return ores::platform::system::get_host_name();
}

}
