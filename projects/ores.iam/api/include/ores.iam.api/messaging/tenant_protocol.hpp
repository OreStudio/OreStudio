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

#include "ores.iam.api/domain/tenant.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

struct get_tenants_request {
    using response_type = struct get_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.list";
    bool include_deleted = false;
};

struct get_tenants_response {
    std::vector<ores::iam::domain::tenant> tenants;
};

struct save_tenant_request {
    using response_type = struct save_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.save";
    ores::iam::domain::tenant data;

    static save_tenant_request from(ores::iam::domain::tenant t) {
        return {.data = std::move(t)};
    }
};

struct save_tenant_response {
    bool success = false;
    std::string message;
};

struct delete_tenant_request {
    using response_type = struct delete_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.delete";
    std::vector<std::string> ids;
};

struct delete_tenant_response {
    bool success = false;
    std::string message;
};

struct get_tenant_history_request {
    using response_type = struct get_tenant_history_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.history";
    std::string id;
};

struct get_tenant_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::iam::domain::tenant> versions;
};

struct complete_tenant_provisioning_command {
    using response_type = struct complete_tenant_provisioning_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.complete-provisioning";
};

struct complete_tenant_provisioning_response {
    bool success = false;
    std::string message;
};

// --- Acme one-click tenant provisioning (--source acme) ---
//
// A single server-side orchestrated request: imports the four-party Acme
// Bank LEI hierarchy, publishes real GLEIF counterparties (small), then
// for each operating company publishes its business units, portfolios,
// books, accounts, and account contact informations. No repeated
// per-party logins, no orchestration logic client-side -- see
// ores_iam_provision_acme_tenant_fn.
struct provision_acme_tenant_command {
    using response_type = struct provision_acme_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.provision-acme";
};

struct provision_acme_tenant_step {
    std::string step;
    std::string action;
    std::uint64_t record_count = 0;
};

struct provision_acme_tenant_response {
    bool success = false;
    std::string message;
    std::vector<provision_acme_tenant_step> steps;
};

}

#endif
