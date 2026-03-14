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
#ifndef ORES_IAM_MESSAGING_TENANT_STATUS_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_STATUS_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.iam/domain/tenant_status.hpp"

namespace ores::iam::messaging {

struct get_tenant_statuses_request {
    using response_type = struct get_tenant_statuses_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenant-statuses.list";
};

struct get_tenant_statuses_response {
    std::vector<ores::iam::domain::tenant_status> statuses;
};

struct save_tenant_status_request {
    using response_type = struct save_tenant_status_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenant-statuses.save";
    ores::iam::domain::tenant_status data;
};

struct save_tenant_status_response {
    bool success = false;
    std::string message;
};

struct delete_tenant_status_request {
    using response_type = struct delete_tenant_status_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenant-statuses.delete";
    std::string status;
};

struct delete_tenant_status_response {
    bool success = false;
    std::string message;
};

struct get_tenant_status_history_request {
    using response_type = struct get_tenant_status_history_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenant-statuses.history";
    std::string status;
};

struct get_tenant_status_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::tenant_status> history;
};

}

#endif
