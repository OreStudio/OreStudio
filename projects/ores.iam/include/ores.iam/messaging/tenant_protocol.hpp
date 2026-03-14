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
#ifndef ORES_IAM_MESSAGING_TENANT_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.iam/domain/tenant.hpp"

namespace ores::iam::messaging {

struct get_tenants_request {
    using response_type = struct get_tenants_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenants.list";
    bool include_deleted = false;
};

struct get_tenants_response {
    std::vector<ores::iam::domain::tenant> tenants;
};

struct save_tenant_request {
    using response_type = struct save_tenant_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenants.save";
    ores::iam::domain::tenant data;

    static save_tenant_request from(ores::iam::domain::tenant t) {
        return { .data = std::move(t) };
    }
};

struct save_tenant_response {
    bool success = false;
    std::string message;
};

struct delete_tenant_request {
    using response_type = struct delete_tenant_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenants.delete";
    std::vector<std::string> ids;
};

struct delete_tenant_response {
    bool success = false;
    std::string message;
};

struct get_tenant_history_request {
    using response_type = struct get_tenant_history_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.tenants.history";
    std::string id;
};

struct get_tenant_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::tenant> versions;
};

}

#endif
