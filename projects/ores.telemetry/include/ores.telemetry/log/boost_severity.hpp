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
#ifndef ORES_TELEMETRY_LOG_BOOST_SEVERITY_HPP
#define ORES_TELEMETRY_LOG_BOOST_SEVERITY_HPP

// Forwarding header - types moved to ores.logging
#include "ores.logging/boost_severity.hpp"

// Also import domain::severity_level for backward compatibility
#include "ores.telemetry/domain/severity_level.hpp"

namespace ores::telemetry::log {

using ores::logging::boost_severity;
using ores::logging::to_boost_severity;
using ores::logging::to_domain_severity;

// Import enum values (C++03 enum values are in enclosing namespace)
using ores::logging::trace;
using ores::logging::debug;
using ores::logging::info;
using ores::logging::warn;
using ores::logging::error;

}

#endif
