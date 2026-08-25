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
#ifndef ORES_COMPUTE_API_MESSAGING_APP_VERSION_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_APP_VERSION_PROTOCOL_HPP

#include "ores.compute.api/domain/app_version.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct get_app_versions_request {
    using response_type = struct get_app_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_versions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_app_versions_response {
    std::vector<ores::compute::domain::app_version> app_versions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_app_version_request {
    using response_type = struct save_app_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_versions.save";
    ores::compute::domain::app_version data;

    static save_app_version_request from(ores::compute::domain::app_version v) {
        return {.data = std::move(v)};
    }
};

struct save_app_version_response {
    bool success = false;
    std::string message;
};

struct delete_app_version_request {
    using response_type = struct delete_app_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_versions.delete";
    std::vector<std::string> ids;
};

struct delete_app_version_response {
    bool success = false;
    std::string message;
};

struct get_app_version_history_request {
    using response_type = struct get_app_version_history_response;
    static constexpr std::string_view nats_subject = "compute.v1.app_versions.history";
    std::string id;
};

struct get_app_version_history_response {
    std::vector<ores::compute::domain::app_version> history;
    bool success = false;
    std::string message;
};

}

#endif
