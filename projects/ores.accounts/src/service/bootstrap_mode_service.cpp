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

#include "ores.accounts/service/bootstrap_mode_service.hpp"
#include <algorithm>

namespace ores::accounts::service {

using namespace ores::utility::log;

bootstrap_mode_service::bootstrap_mode_service(utility::repository::context ctx)
    : account_repo_(ctx),
      feature_flags_repo_(ctx),
      ctx_(ctx) {

    BOOST_LOG_SEV(lg(), debug) << "DML for account: " << account_repo_.sql();
    BOOST_LOG_SEV(lg(), debug) << "DML for feature_flags: " << feature_flags_repo_.sql();
}

bool bootstrap_mode_service::is_in_bootstrap_mode() {
    BOOST_LOG_SEV(lg(), debug) << "Checking bootstrap mode status";

    auto flags = feature_flags_repo_.read_latest(BOOTSTRAP_FLAG_NAME);
    if (flags.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Bootstrap flag not found, assuming secure mode";
        return false;
    }

    const bool in_bootstrap = flags[0].enabled;
    BOOST_LOG_SEV(lg(), debug) << "Bootstrap mode: " << (in_bootstrap ? "true" : "false");
    return in_bootstrap;
}

void bootstrap_mode_service::initialize_bootstrap_state() {
    BOOST_LOG_SEV(lg(), info) << "Initializing bootstrap mode state";

    auto flags = feature_flags_repo_.read_latest(BOOTSTRAP_FLAG_NAME);
    auto accounts = account_repo_.read_latest();

    bool admin_exists = std::any_of(accounts.begin(), accounts.end(),
        [](const domain::account& acc) { return acc.is_admin; });

    BOOST_LOG_SEV(lg(), debug) << "Total accounts: " << accounts.size();
    BOOST_LOG_SEV(lg(), debug) << "Admin exists: " << (admin_exists ? "true" : "false");

    if (flags.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Bootstrap flag does not exist, creating it";

        variability::domain::feature_flags bootstrap_flag{
            .enabled = !admin_exists,
            .name = BOOTSTRAP_FLAG_NAME,
            .description = "Indicates whether the system is in bootstrap mode (waiting for initial admin account)",
            .modified_by = "system"
        };

        feature_flags_repo_.write(bootstrap_flag);
        BOOST_LOG_SEV(lg(), info) << "Created bootstrap flag with enabled="
                                  << (bootstrap_flag.enabled ? "true" : "false");
    } else {
        const bool flag_enabled = flags[0].enabled;
        BOOST_LOG_SEV(lg(), debug) << "Bootstrap flag exists with enabled="
                                   << (flag_enabled ? "true" : "false");

        if (flag_enabled && admin_exists) {
            BOOST_LOG_SEV(lg(), warn) << "Bootstrap flag is enabled but admin accounts exist, correcting state";
            exit_bootstrap_mode();
        } else if (!flag_enabled && !admin_exists) {
            BOOST_LOG_SEV(lg(), warn) << "Bootstrap flag is disabled but no admin accounts exist, this is inconsistent";
            BOOST_LOG_SEV(lg(), warn) << "Enabling bootstrap mode";

            feature_flags_repo_.remove(BOOTSTRAP_FLAG_NAME);

            variability::domain::feature_flags bootstrap_flag{
                .enabled = true,
                .name = BOOTSTRAP_FLAG_NAME,
                .description = "Indicates whether the system is in bootstrap mode (waiting for initial admin account)",
                .modified_by = "system"
            };
            feature_flags_repo_.write(bootstrap_flag);
        }
    }
}

void bootstrap_mode_service::exit_bootstrap_mode() {
    BOOST_LOG_SEV(lg(), info) << "Exiting bootstrap mode";

    auto flags = feature_flags_repo_.read_latest(BOOTSTRAP_FLAG_NAME);
    if (flags.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Bootstrap flag does not exist, cannot exit bootstrap mode";
        return;
    }

    if (!flags[0].enabled) {
        BOOST_LOG_SEV(lg(), debug) << "Already in secure mode, nothing to do";
        return;
    }

    feature_flags_repo_.remove(BOOTSTRAP_FLAG_NAME);

    variability::domain::feature_flags secure_mode_flag{
        .enabled = false,
        .name = BOOTSTRAP_FLAG_NAME,
        .description = "Indicates whether the system is in bootstrap mode (waiting for initial admin account)",
        .modified_by = "system"
    };

    feature_flags_repo_.write(secure_mode_flag);
    BOOST_LOG_SEV(lg(), info) << "Successfully exited bootstrap mode";
}

}
