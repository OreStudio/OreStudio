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
#ifndef ORES_TELEMETRY_DOMAIN_RESOURCE_HPP
#define ORES_TELEMETRY_DOMAIN_RESOURCE_HPP

#include <cstdint>
#include <optional>
#include <string>
#include "ores.telemetry/domain/attribute_value.hpp"

namespace ores::telemetry::domain {

/**
 * @brief Represents the entity producing telemetry data.
 *
 * A resource describes the source of telemetry data, typically representing
 * a machine, service instance, or process. Resources are created once at
 * startup and shared across all telemetry data produced by that entity.
 *
 * This follows OpenTelemetry's resource semantic conventions.
 */
struct resource final {
    /**
     * @brief Key-value pairs describing the resource.
     *
     * Standard OpenTelemetry resource attributes include:
     * - service.name: Logical name of the service
     * - service.version: Version of the service
     * - service.instance.id: Unique instance identifier
     * - host.name: Hostname of the machine
     * - host.id: Unique host identifier (locally derived)
     * - process.pid: Process ID
     */
    attributes attrs;

    /**
     * @brief Gets the service name attribute if present.
     */
    std::optional<std::string> service_name() const;

    /**
     * @brief Gets the host name attribute if present.
     */
    std::optional<std::string> host_name() const;

    /**
     * @brief Gets the host ID (machine identifier) if present.
     */
    std::optional<std::string> host_id() const;

    /**
     * @brief Creates a resource by detecting the local environment.
     *
     * This performs local derivation of the machine identity by hashing
     * stable system properties (hostname, MAC addresses, etc.).
     *
     * @param service_name The logical name of the service.
     * @param service_version The version of the service.
     * @return A resource populated with detected attributes.
     */
    static resource from_environment(std::string_view service_name,
                                      std::string_view service_version);
};

}

#endif
