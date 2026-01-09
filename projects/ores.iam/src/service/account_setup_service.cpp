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
#include "ores.iam/service/account_setup_service.hpp"
#include "ores.iam/domain/role.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::service {

using namespace ores::logging;

account_setup_service::account_setup_service(account_service& account_svc,
    std::shared_ptr<authorization_service> auth_svc)
    : account_svc_(account_svc), auth_svc_(std::move(auth_svc)) {}

domain::account account_setup_service::create_account(const std::string& username,
    const std::string& email, const std::string& password,
    const std::string& recorded_by,
    const std::string& change_commentary) {

    return create_account_with_role(username, email, password, recorded_by,
        domain::roles::viewer, change_commentary);
}

domain::account account_setup_service::create_account_with_role(
    const std::string& username, const std::string& email,
    const std::string& password, const std::string& recorded_by,
    const std::string& role_name,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), info) << "Creating account '" << username
                              << "' with role '" << role_name << "'";

    // Step 1: Look up the role FIRST to fail fast if RBAC isn't seeded
    auto role = auth_svc_->find_role_by_name(role_name);
    if (!role) {
        BOOST_LOG_SEV(lg(), error) << "Role '" << role_name
                                   << "' not found - RBAC may not be properly seeded";
        throw std::runtime_error("Role '" + role_name +
            "' not found. Ensure RBAC is properly seeded.");
    }

    // Step 2: Create the account (and login_info)
    domain::account account = account_svc_.create_account(
        username, email, password, recorded_by, change_commentary);

    BOOST_LOG_SEV(lg(), debug) << "Account created with ID: "
                               << boost::uuids::to_string(account.id);

    // Step 3: Assign the role to the account
    auth_svc_->assign_role(account.id, role->id, recorded_by);

    BOOST_LOG_SEV(lg(), info) << "Assigned role '" << role_name
                              << "' to account '" << username << "'";

    return account;
}

}
