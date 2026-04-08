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
#ifndef ORES_PLATFORM_PROCESS_SIGNALS_HPP
#define ORES_PLATFORM_PROCESS_SIGNALS_HPP

#include <array>
#include <csignal>

namespace ores::platform::process {

// Signals that trigger graceful shutdown.
// On POSIX: SIGINT, SIGTERM, SIGQUIT — supports the stop-escalation
// sequence SIGTERM → SIGQUIT → SIGKILL used by process supervisors.
// On Windows: SIGINT and SIGTERM only (SIGQUIT is not defined).
#ifndef _WIN32
inline constexpr std::array<int, 3> shutdown_signals = { SIGINT, SIGTERM, SIGQUIT };
#else
inline constexpr std::array<int, 2> shutdown_signals = { SIGINT, SIGTERM };
#endif

} // namespace ores::platform::process

#endif
