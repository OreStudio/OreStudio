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
#ifndef ORES_COMPUTE_MESSAGING_APP_VERSION_PROTOCOL_HPP
#define ORES_COMPUTE_MESSAGING_APP_VERSION_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.api/domain/app_version_platform.hpp"

namespace ores::compute::messaging {

struct list_app_versions_request {
    using response_type = struct list_app_versions_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app-versions.list";
    int offset = 0;
    int limit = 100;
};

struct list_app_versions_response {
    std::vector<ores::compute::domain::app_version> app_versions;
    int total_available_count = 0;
};

struct save_app_version_request {
    using response_type = struct save_app_version_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app-versions.save";
    ores::compute::domain::app_version app_version;
    std::vector<ores::compute::domain::app_version_platform> platforms;
    std::string change_reason_code;
    std::string change_commentary;
};

struct save_app_version_response {
    bool success = false;
    std::string message;
};

struct get_app_version_history_request {
    using response_type = struct get_app_version_history_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.app-versions.history";
    std::string id;
};

struct get_app_version_history_response {
    bool success = true;
    std::string message;
    std::vector<ores::compute::domain::app_version> versions;
};

}

#endif
