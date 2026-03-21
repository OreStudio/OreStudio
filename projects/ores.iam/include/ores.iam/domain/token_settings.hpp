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
#ifndef ORES_IAM_DOMAIN_TOKEN_SETTINGS_HPP
#define ORES_IAM_DOMAIN_TOKEN_SETTINGS_HPP

#include "ores.variability/service/system_settings_service.hpp"

namespace ores::iam::domain {

/**
 * @brief Typed representation of the JWT token lifetime settings.
 *
 * Loaded from the system settings service at startup and reloaded
 * whenever any iam.token.* setting changes.
 */
struct token_settings {
    /**
     * @brief Lifetime in seconds of every issued JWT access token.
     *
     * Default: 1800 (30 minutes).
     */
    int access_lifetime_s = 1800;

    /**
     * @brief Lifetime in seconds of the party-selection step token.
     *
     * Default: 300 (5 minutes).
     */
    int party_selection_lifetime_s = 300;

    /**
     * @brief Hard ceiling in seconds for a session regardless of refresh.
     *
     * Default: 28800 (8 hours).
     */
    int max_session_s = 28800;

    /**
     * @brief Percentage of token lifetime at which to proactively refresh.
     *
     * Default: 80 (80%).
     */
    int refresh_threshold_pct = 80;

    /**
     * @brief Loads token settings from the system settings service.
     *
     * Falls back to the struct defaults (which match the registry defaults)
     * if any setting is absent from the database.
     */
    [[nodiscard]] static token_settings
    load(variability::service::system_settings_service& svc);
};

}

#endif
