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

#ifndef ORES_ACCOUNTS_SERVICE_BOOTSTRAP_MODE_SERVICE_HPP
#define ORES_ACCOUNTS_SERVICE_BOOTSTRAP_MODE_SERVICE_HPP

#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts/repository/feature_flags_repository.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/repository/context.hpp"

namespace ores::accounts::service {

/**
 * @brief Service for managing system bootstrap mode state.
 *
 * The bootstrap_mode_service manages the system's bootstrap mode, which is
 * a special state where only the initial administrator account can be created.
 * The service uses the feature_flags table to persist the bootstrap mode state
 * with the flag name "system.bootstrap_mode".
 *
 * Bootstrap mode logic:
 * - enabled=true: System is in bootstrap mode (no admin accounts exist)
 * - enabled=false: System is in secure mode (admin account exists)
 *
 * The service provides methods to:
 * - Check if the system is currently in bootstrap mode
 * - Initialize bootstrap state on system startup
 * - Transition from bootstrap mode to secure mode
 */
class bootstrap_mode_service {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.accounts.service.bootstrap_mode_service");
        return instance;
    }

    static constexpr const char* BOOTSTRAP_FLAG_NAME = "system.bootstrap_mode";

public:
    using context = ores::utility::repository::context;

    /**
     * @brief Constructs a bootstrap_mode_service with required repositories.
     *
     * @param ctx The database context for repository access
     */
    explicit bootstrap_mode_service(utility::repository::context ctx);

    /**
     * @brief Checks if the system is currently in bootstrap mode.
     *
     * Queries the feature_flags table for the system.bootstrap_mode flag.
     * If the flag is enabled (true), the system is in bootstrap mode.
     *
     * @return true if system is in bootstrap mode, false otherwise
     */
    bool is_in_bootstrap_mode();

    /**
     * @brief Initializes the bootstrap mode state on system startup.
     *
     * This method should be called during service initialization. It ensures
     * the bootstrap mode feature flag exists and is set correctly based on
     * whether admin accounts exist in the system.
     *
     * Logic:
     * - If flag doesn't exist and no admin accounts exist: create flag with enabled=true
     * - If flag doesn't exist and admin accounts exist: create flag with enabled=false
     * - If flag exists: verify it matches reality (admin accounts exist = flag should be false)
     */
    void initialize_bootstrap_state();

    /**
     * @brief Transitions the system from bootstrap mode to secure mode.
     *
     * This method is called after the first admin account is successfully created.
     * It updates the system.bootstrap_mode feature flag from enabled=true to
     * enabled=false using bitemporal versioning.
     *
     * This operation is idempotent - calling it when already in secure mode
     * has no effect.
     */
    void exit_bootstrap_mode();

private:
    repository::account_repository account_repo_;
    repository::feature_flags_repository feature_flags_repo_;
    utility::repository::context ctx_;
};

}

#endif
