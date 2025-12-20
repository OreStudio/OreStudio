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
#include "ores.telemetry/domain/resource.hpp"
#include "ores.platform/net/network_info.hpp"

namespace ores::telemetry::domain {

std::optional<std::string> resource::service_name() const {
    auto it = attrs.find("service.name");
    if (it != attrs.end()) {
        if (auto* str = std::get_if<std::string>(&it->second)) {
            return *str;
        }
    }
    return std::nullopt;
}

std::optional<std::string> resource::host_name() const {
    auto it = attrs.find("host.name");
    if (it != attrs.end()) {
        if (auto* str = std::get_if<std::string>(&it->second)) {
            return *str;
        }
    }
    return std::nullopt;
}

std::optional<std::string> resource::host_id() const {
    auto it = attrs.find("host.id");
    if (it != attrs.end()) {
        if (auto* str = std::get_if<std::string>(&it->second)) {
            return *str;
        }
    }
    return std::nullopt;
}

resource resource::from_environment(std::string_view service_name,
                                    std::string_view service_version) {
    resource res;

    // Service attributes
    res.attrs["service.name"] = std::string(service_name);
    res.attrs["service.version"] = std::string(service_version);

    // Host attributes (using cross-platform utility)
    res.attrs["host.name"] = platform::net::get_hostname();
    res.attrs["host.id"] = platform::net::derive_machine_id();

    // Process attributes
    res.attrs["process.pid"] = platform::net::get_process_id();

    return res;
}

}
