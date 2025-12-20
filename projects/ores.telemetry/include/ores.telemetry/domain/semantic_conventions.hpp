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
#ifndef ORES_TELEMETRY_DOMAIN_SEMANTIC_CONVENTIONS_HPP
#define ORES_TELEMETRY_DOMAIN_SEMANTIC_CONVENTIONS_HPP

#include <string_view>

namespace ores::telemetry::domain::semconv {

/**
 * @brief Semantic convention constants for telemetry attributes.
 *
 * These constants follow OpenTelemetry semantic conventions where applicable,
 * with additional project-specific conventions as needed.
 */

// Link relationship types
namespace link {

constexpr std::string_view relationship = "link.relationship";
constexpr std::string_view triggered_by = "triggered_by";
constexpr std::string_view follows_from = "follows_from";
constexpr std::string_view caused_by = "caused_by";

}

// Session attributes
namespace session {

constexpr std::string_view id = "session.id";

}

// Service attributes (OTel standard)
namespace service {

constexpr std::string_view name = "service.name";
constexpr std::string_view version = "service.version";
constexpr std::string_view instance_id = "service.instance.id";

}

// Host attributes (OTel standard)
namespace host {

constexpr std::string_view name = "host.name";
constexpr std::string_view id = "host.id";

}

// Process attributes (OTel standard)
namespace process {

constexpr std::string_view pid = "process.pid";

}

}

#endif
