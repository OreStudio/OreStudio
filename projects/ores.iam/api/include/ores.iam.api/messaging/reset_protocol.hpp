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
#ifndef ORES_IAM_MESSAGING_RESET_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_RESET_PROTOCOL_HPP

#include <string>

namespace ores::iam::messaging {

// Reset a single tenant to bootstrap state.
// Soft-deletes admin-created data, flips Operational parties to Inactive,
// and re-enables system.bootstrap_mode so both TenantProvisioningWizard
// and PartyProvisioningWizard fire on next login.
// Requires: iam::system:reset-tenant permission (SuperAdmin only).
struct reset_tenant_command {
    using response_type = struct reset_tenant_result;
    static constexpr std::string_view nats_subject = "iam.v1.system.reset-tenant";
    std::string tenant_code;
};

struct reset_tenant_result {
    bool success = false;
    std::string message;
};

// Reset the entire system to pre-bootstrap state.
// Purges all non-system tenants (hard delete), removes system admin accounts,
// and re-enables system.bootstrap_mode so SystemProvisionerWizard fires on
// next startup.
// Requires: iam::system:reset permission (SuperAdmin only).
struct reset_system_command {
    using response_type = struct reset_system_result;
    static constexpr std::string_view nats_subject = "iam.v1.system.reset";
};

struct reset_system_result {
    bool success = false;
    std::string message;
};

}

#endif
