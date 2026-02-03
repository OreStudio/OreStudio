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

#include "ores.iam/service/bootstrap_mode_service.hpp"

#include <algorithm>
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::iam::service {

using namespace ores::logging;
namespace reason = database::domain::change_reason_constants;

bootstrap_mode_service::bootstrap_mode_service(database::context ctx,
    std::string tenant_id,
    std::shared_ptr<authorization_service> auth_service)
    : account_repo_(ctx),
      system_flags_service_(ctx, std::move(tenant_id)),
      auth_service_(std::move(auth_service)),
      ctx_(ctx) {
    BOOST_LOG_SEV(lg(), debug) << "DML for account: " << account_repo_.sql();
    system_flags_service_.refresh();
}

bool bootstrap_mode_service::is_in_bootstrap_mode() {
    BOOST_LOG_SEV(lg(), debug) << "Checking bootstrap mode status";

    const bool in_bootstrap = system_flags_service_.is_bootstrap_mode_enabled();
    BOOST_LOG_SEV(lg(), debug) << "Bootstrap mode: "
        << (in_bootstrap ? "true" : "false");
    return in_bootstrap;
}

void bootstrap_mode_service::initialize_bootstrap_state() {
    BOOST_LOG_SEV(lg(), info) << "Initializing bootstrap mode state";

    // Check if any account has the Admin role via RBAC
    auto accounts = account_repo_.read_latest();
    auto admin_role = auth_service_->find_role_by_name(domain::roles::admin);
    bool admin_exists = false;

    if (admin_role) {
        admin_exists = std::ranges::any_of(accounts,
            [this, &admin_role](const domain::account& acc) {
                auto roles = auth_service_->get_account_roles(acc.id);
                return std::ranges::any_of(roles,
                    [&admin_role](const domain::role& r) {
                        return r.id == admin_role->id;
                    });
            });
    }

    BOOST_LOG_SEV(lg(), debug) << "Total accounts: " << accounts.size();
    BOOST_LOG_SEV(lg(), debug) << "Admin exists: "
        << (admin_exists ? "true" : "false");

    const bool flag_enabled = system_flags_service_.is_bootstrap_mode_enabled();
    BOOST_LOG_SEV(lg(), debug) << "Bootstrap flag enabled: "
        << (flag_enabled ? "true" : "false");

    if (flag_enabled && admin_exists) {
        BOOST_LOG_SEV(lg(), warn)
            << "Bootstrap flag is enabled but admin accounts exist, "
            << "correcting state";
        exit_bootstrap_mode();
    } else if (!flag_enabled && !admin_exists) {
        BOOST_LOG_SEV(lg(), warn)
            << "Bootstrap flag is disabled but no admin accounts exist, "
            << "this is inconsistent. Enabling bootstrap mode";
        system_flags_service_.set_bootstrap_mode(true, "system",
            std::string{reason::codes::new_record}, "Bootstrap mode enabled due to inconsistent state - no admin accounts exist");
    }
}

void bootstrap_mode_service::exit_bootstrap_mode() {
    BOOST_LOG_SEV(lg(), info) << "Exiting bootstrap mode";

    if (!system_flags_service_.is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), debug) << "Already in secure mode, nothing to do";
        return;
    }

    system_flags_service_.set_bootstrap_mode(false, "system",
        std::string{reason::codes::new_record}, "Bootstrap mode disabled - system now in secure mode");
    BOOST_LOG_SEV(lg(), info) << "Successfully exited bootstrap mode";
}

}
