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
#ifndef ORES_IAM_API_MESSAGING_TENANT_STATUS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_STATUS_PROTOCOL_HPP

#include "ores.iam.api/domain/tenant_status.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct get_tenant_statuses_request {
    using response_type = struct get_tenant_statuses_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_tenant_statuses_response {
    std::vector<ores::iam::domain::tenant_status> statuses;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_tenant_status_request {
    using response_type = struct save_tenant_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.save";
    ores::iam::domain::tenant_status data;

    static save_tenant_status_request from(ores::iam::domain::tenant_status v) {
        return {.data = std::move(v)};
    }
};

struct save_tenant_status_response {
    bool success = false;
    std::string message;
};

struct delete_tenant_status_request {
    using response_type = struct delete_tenant_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.delete";
    std::vector<std::string> statuss;
};

struct delete_tenant_status_response {
    bool success = false;
    std::string message;
};

struct get_tenant_status_history_request {
    using response_type = struct get_tenant_status_history_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenant_statuses.history";
    std::string status;
};

struct get_tenant_status_history_response {
    std::vector<ores::iam::domain::tenant_status> history;
    bool success = false;
    std::string message;
};

}

#endif
