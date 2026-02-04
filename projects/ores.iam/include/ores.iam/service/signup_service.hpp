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
#ifndef ORES_IAM_SERVICE_SIGNUP_SERVICE_HPP
#define ORES_IAM_SERVICE_SIGNUP_SERVICE_HPP

#include <memory>
#include <string>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.iam/domain/account.hpp"
#include "ores.iam/repository/account_repository.hpp"
#include "ores.iam/repository/login_info_repository.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::iam::service {

/**
 * @brief Result of a signup operation.
 */
struct signup_result {
    bool success = false;
    std::string error_message;
    ores::utility::serialization::error_code error_code = ores::utility::serialization::error_code::none;
    boost::uuids::uuid account_id;
    std::string username;
};

/**
 * @brief Service for user self-registration (signup).
 *
 * This service handles the signup workflow, allowing users to create their
 * own accounts when self-registration is enabled via the system.user_signups
 * feature flag.
 *
 * The service validates:
 * - Feature flags (signup enabled, authorization not required)
 * - Username uniqueness
 * - Email uniqueness and format
 * - Password policy compliance
 */
class signup_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.signup_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a signup_service.
     *
     * @param ctx The database context for repository access.
     * @param system_flags Shared system flags service for flag access.
     * @param auth_service Shared authorization service for role assignment.
     */
    signup_service(database::context ctx,
        std::shared_ptr<variability::service::system_flags_service> system_flags,
        std::shared_ptr<authorization_service> auth_service);

    /**
     * @brief Registers a new user account.
     *
     * This method performs the full signup workflow:
     * 1. Checks if signups are enabled (system.user_signups flag)
     * 2. Checks if authorization is required (fails if enabled - not implemented)
     * 3. Validates username uniqueness
     * 4. Validates email format and uniqueness
     * 5. Validates password against policy
     * 6. Creates the account
     *
     * @param username The desired username (must be unique)
     * @param email The user's email address (must be unique and valid format)
     * @param password The password (must meet policy requirements)
     * @return signup_result with success status and account details or error info
     */
    signup_result register_user(const std::string& username,
        const std::string& email, const std::string& password);

    /**
     * @brief Checks if signups are currently enabled.
     *
     * @return true if the system.user_signups flag is enabled
     */
    bool is_signup_enabled() const;

private:
    repository::account_repository account_repo_;
    repository::login_info_repository login_info_repo_;
    std::shared_ptr<variability::service::system_flags_service> system_flags_;
    std::shared_ptr<authorization_service> auth_service_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
