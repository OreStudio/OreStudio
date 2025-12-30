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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_PLATFORM_HPP
#define ORES_PLATFORM_HPP

/**
 * @brief Platform abstraction layer for ORE Studio.
 *
 * This module provides cross-platform abstractions for system-level operations,
 * isolating platform-specific code from the rest of the codebase. Key features
 * include:
 *
 * - Environment: access to environment variables and system information
 * - Filesystem: file operations, path handling, and I/O error types
 * - Network: network interface information and utilities
 * - Time: datetime utilities, time point parsing, and relative time formatting
 *
 * The module is organized into namespaces: environment (system environment),
 * filesystem (file operations), net (network utilities), and time (datetime
 * handling).
 */
namespace ores::platform {}

#endif
