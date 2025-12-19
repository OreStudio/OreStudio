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
#ifndef ORES_VARIABILITY_DOMAIN_SYSTEM_FLAGS_CACHE_HPP
#define ORES_VARIABILITY_DOMAIN_SYSTEM_FLAGS_CACHE_HPP

namespace ores::variability::domain {

/**
 * @brief POD containing cached values of well-known system flags.
 *
 * This struct holds a snapshot of system flag values, typically read from
 * the database at startup or refreshed periodically. It provides fast,
 * lock-free access to flag values without database queries.
 *
 * Default values match those defined in system_flag_definitions.
 */
struct system_flags_cache {
    /**
     * @brief Whether the system is in bootstrap mode.
     *
     * When true, the system is waiting for the initial admin account.
     * Default: true (matches system_flag_definitions)
     */
    bool bootstrap_mode = true;

    /**
     * @brief Whether user self-registration is enabled.
     *
     * When true, users can create their own accounts.
     * Default: false (matches system_flag_definitions)
     */
    bool user_signups = false;

    /**
     * @brief Whether new signups require admin authorization.
     *
     * When true, newly created accounts require admin approval.
     * Note: Not yet implemented - enabling will cause signup to fail.
     * Default: false (matches system_flag_definitions)
     */
    bool signup_requires_authorization = false;

    /**
     * @brief Whether password validation is disabled.
     *
     * When true, strict password validation requirements are bypassed.
     * FOR TESTING/DEVELOPMENT ONLY.
     * Default: false (matches system_flag_definitions)
     */
    bool disable_password_validation = false;
};

}

#endif
