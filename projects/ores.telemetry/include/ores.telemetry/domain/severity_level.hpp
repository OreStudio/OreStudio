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
#ifndef ORES_TELEMETRY_DOMAIN_SEVERITY_LEVEL_HPP
#define ORES_TELEMETRY_DOMAIN_SEVERITY_LEVEL_HPP

#include <cstdint>

namespace ores::telemetry::domain {

/**
 * @brief Log severity levels following OpenTelemetry conventions.
 *
 * These severity levels are compatible with OpenTelemetry's log data model
 * and map to standard logging frameworks. The numeric values match the
 * OpenTelemetry specification for interoperability.
 */
enum class severity_level : std::uint8_t {
    trace = 1,
    debug = 5,
    info = 9,
    warn = 13,
    error = 17,
    fatal = 21
};

}

#endif
