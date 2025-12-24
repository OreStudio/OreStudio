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
#ifndef ORES_IAM_SERVICE_ACCOUNT_SETUP_SERVICE_HPP
#define ORES_IAM_SERVICE_ACCOUNT_SETUP_SERVICE_HPP

#include <string>
#include <memory>
#include "ores.iam/domain/account.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::iam::service {

/**
 * @brief Centralized service for complete account initialization.
 *
 * This service orchestrates the full account creation workflow:
 * 1. Creates the account record and login_info via account_service
 * 2. Assigns the default Viewer role via authorization_service
 *
 * Use this service instead of calling account_service::create_account()
 * directly to ensure accounts are properly initialized with roles.
 */
class account_setup_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.account_setup_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs an account_setup_service with required dependencies.
     *
     * @param account_service The service for account creation
     * @param auth_service The service for role assignment
     */
    account_setup_service(account_service& account_svc,
        std::shared_ptr<authorization_service> auth_svc);

    /**
     * @brief Creates a new account with the default Viewer role.
     *
     * This method:
     * 1. Creates the account via account_service
     * 2. Looks up the Viewer role
     * 3. Assigns the Viewer role to the new account
     *
     * @param username The unique username for the account
     * @param email The email address for the account
     * @param password The plaintext password (will be hashed)
     * @param recorded_by The username of the person creating the account
     * @return The created account with the Viewer role assigned
     * @throws std::runtime_error If Viewer role is not found (RBAC not seeded)
     */
    domain::account create_account(const std::string& username,
        const std::string& email, const std::string& password,
        const std::string& recorded_by);

    /**
     * @brief Creates a new account with a specific role.
     *
     * This method:
     * 1. Creates the account via account_service
     * 2. Assigns the specified role to the new account
     *
     * @param username The unique username for the account
     * @param email The email address for the account
     * @param password The plaintext password (will be hashed)
     * @param recorded_by The username of the person creating the account
     * @param role_name The name of the role to assign
     * @return The created account with the specified role assigned
     * @throws std::runtime_error If the specified role is not found
     */
    domain::account create_account_with_role(const std::string& username,
        const std::string& email, const std::string& password,
        const std::string& recorded_by, const std::string& role_name);

private:
    account_service& account_svc_;
    std::shared_ptr<authorization_service> auth_svc_;
};

}

#endif
