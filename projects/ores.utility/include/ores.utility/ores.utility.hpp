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
#ifndef ORES_UTILITY_ORES_UTILITY_HPP
#define ORES_UTILITY_ORES_UTILITY_HPP

/**
 * @brief Foundation utilities used across all ORE Studio components.
 *
 * Provides core infrastructure and helper functions that don't fit into
 * domain-specific modules. Key namespaces:
 *
 * - converter: Base64 and Base32 encoding/decoding utilities
 * - datetime: Date/time formatting and parsing utilities
 * - faker: Test data generation (TOTP secrets, datetime, network endpoints)
 * - geo: IP geolocation services using MaxMind database
 * - program_options: Environment variable mapping for Boost.Program_options
 * - rfl: Custom reflect-cpp reflectors for common types
 * - streaming: iostream operators for std types
 * - string: Type conversion and string manipulation utilities
 * - uuid: UUID v7 generation for time-ordered unique identifiers
 * - version: Application version utilities
 *
 * Note: Platform abstractions (filesystem, environment, time, net) have moved
 * to ores.platform. Database and logging functionality have moved to
 * ores.database and ores.telemetry respectively.
 *
 * This module has no dependencies on other ORE Studio components, making it
 * the foundation layer upon which all other modules are built.
 */
namespace ores::utility {}

#endif
