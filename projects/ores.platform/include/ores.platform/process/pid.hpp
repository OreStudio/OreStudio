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
#ifndef ORES_PLATFORM_PROCESS_PID_HPP
#define ORES_PLATFORM_PROCESS_PID_HPP

#include "ores.platform/export.hpp"
#include <cstdint>

namespace ores::platform::process {

/**
 * @brief Returns the process id of the current process.
 *
 * Cross-platform wrapper over boost::process::v2::current_pid(). The
 * returned value is always positive and stable for the lifetime of the
 * process.
 */
ORES_PLATFORM_EXPORT std::int64_t current_pid();

}

#endif
