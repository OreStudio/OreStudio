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
#ifndef ORES_PLATFORM_PROCESS_EXECUTABLE_HPP
#define ORES_PLATFORM_PROCESS_EXECUTABLE_HPP

#include <filesystem>
#include <string>

namespace ores::platform::process {

/**
 * @brief Returns the path to the currently running executable.
 *
 * Uses /proc/self/exe on Linux, _NSGetExecutablePath on macOS, and
 * GetModuleFileNameW on Windows. Returns an empty path on failure.
 */
std::filesystem::path executable_path();

/**
 * @brief Returns the build time of the running executable as a string.
 *
 * Reads the executable's modification time from the filesystem — the mtime
 * set by the linker when the binary was last built. This is the idiomatic
 * way to obtain build time without embedding a timestamp in the binary
 * (which would cause unnecessary recompilation on every build).
 *
 * Format: "YYYY/MM/DD HH:MM:SS" (UTC). Returns "unknown" on failure.
 */
std::string executable_build_time();

/**
 * @brief Returns the hostname of the current machine.
 *
 * Cross-platform wrapper: gethostname on POSIX, GetComputerNameA on Windows.
 * Returns "unknown" on failure.
 */
std::string get_hostname();

}

#endif
