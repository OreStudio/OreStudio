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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_MESSAGING_PERMISSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_PERMISSION_PROTOCOL_HPP

#include "ores.iam.api/domain/permission.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct get_permissions_request {
    using response_type = struct get_permissions_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_permissions_response {
    std::vector<ores::iam::domain::permission> permissions;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_permission_request {
    using response_type = struct save_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.save";
    ores::iam::domain::permission data;

    static save_permission_request from(ores::iam::domain::permission v) {
        return {.data = std::move(v)};
    }
};

struct save_permission_response {
    bool success = false;
    std::string message;
};

struct delete_permission_request {
    using response_type = struct delete_permission_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.delete";
    std::vector<std::string> ids;
};

struct delete_permission_response {
    bool success = false;
    std::string message;
};

struct get_permission_history_request {
    using response_type = struct get_permission_history_response;
    static constexpr std::string_view nats_subject = "iam.v1.permissions.history";
    std::string id;
};

struct get_permission_history_response {
    std::vector<ores::iam::domain::permission> history;
    bool success = false;
    std::string message;
};

}

#endif
