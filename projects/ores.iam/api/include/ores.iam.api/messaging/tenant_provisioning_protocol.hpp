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
#ifndef ORES_IAM_MESSAGING_TENANT_PROVISIONING_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_PROVISIONING_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

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
// per-party logins, no orchestration logic client-side -- driven by
// internal actor impersonation through the real handler pipeline, see
// ores.iam.core/messaging/tenant_provisioning_handler.hpp's provision_acme.
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
