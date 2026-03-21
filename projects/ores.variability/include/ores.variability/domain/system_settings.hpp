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
#ifndef ORES_VARIABILITY_DOMAIN_SYSTEM_SETTINGS_HPP
#define ORES_VARIABILITY_DOMAIN_SYSTEM_SETTINGS_HPP

#include <array>
#include <string_view>
#include <stdexcept>

namespace ores::variability::domain {

/**
 * @brief Metadata for a well-known system setting.
 *
 * The registry drives: compile-time defaults (returned by service when a
 * setting is absent from the DB), the populate SQL seed script, and the UI
 * description column.
 */
struct system_setting_definition {
    std::string_view name;
    std::string_view data_type;      // "boolean" | "integer" | "string" | "json"
    std::string_view default_value;
    std::string_view description;
};

/**
 * @brief Compile-time registry of all well-known system settings.
 *
 * Modules add their settings here. JWT-related settings are added in the
 * JWT token refresh plan PR.
 */
inline constexpr std::array system_setting_definitions = {
    system_setting_definition{
        .name = "system.bootstrap_mode",
        .data_type = "boolean",
        .default_value = "true",
        .description = "Indicates whether the system is in bootstrap mode "
        "(waiting for initial admin account)."
    },
    system_setting_definition{
        .name = "system.user_signups",
        .data_type = "boolean",
        .default_value = "false",
        .description = "Controls whether user self-registration is allowed."
    },
    system_setting_definition{
        .name = "system.signup_requires_authorization",
        .data_type = "boolean",
        .default_value = "false",
        .description = "Controls whether new signups require admin authorization. "
        "NOT YET IMPLEMENTED - enabling will cause signup to fail."
    },
    system_setting_definition{
        .name = "system.disable_password_validation",
        .data_type = "boolean",
        .default_value = "false",
        .description = "When enabled, disables strict password validation. "
        "FOR TESTING/DEVELOPMENT ONLY."
    },
    system_setting_definition{
        .name = "system.synthetic_data_generation",
        .data_type = "boolean",
        .default_value = "false",
        .description = "Enables synthetic test data generation in the UI. "
        "FOR TESTING/DEVELOPMENT ONLY."
    },
    // -------------------------------------------------------------------------
    // IAM token lifetime settings
    // -------------------------------------------------------------------------
    system_setting_definition{
        .name = "iam.token.access_lifetime_seconds",
        .data_type = "integer",
        .default_value = "1800",
        .description = "Lifetime in seconds of every issued JWT access token. "
        "Default is 1800 (30 minutes)."
    },
    system_setting_definition{
        .name = "iam.token.party_selection_lifetime_seconds",
        .data_type = "integer",
        .default_value = "300",
        .description = "Lifetime in seconds of the short-lived party-selection "
        "step token. Default is 300 (5 minutes)."
    },
    system_setting_definition{
        .name = "iam.token.max_session_seconds",
        .data_type = "integer",
        .default_value = "28800",
        .description = "Hard ceiling in seconds after which a session must "
        "re-authenticate regardless of refresh activity. Default is 28800 (8 hours)."
    },
    system_setting_definition{
        .name = "iam.token.refresh_threshold_pct",
        .data_type = "integer",
        .default_value = "80",
        .description = "Percentage of token lifetime at which the client "
        "proactively requests a token refresh. Default is 80 (80%)."
    }
};

/**
 * @brief Looks up a setting definition by name.
 *
 * @throws std::out_of_range if name is not registered.
 */
[[nodiscard]] inline const system_setting_definition&
get_setting_definition(std::string_view name) {
    for (const auto& def : system_setting_definitions) {
        if (def.name == name)
            return def;
    }
    throw std::out_of_range(
        std::string("System setting definition not found: ") + std::string(name));
}

/**
 * @brief Returns the default value for a registered setting name.
 *
 * @throws std::out_of_range if name is not registered.
 */
[[nodiscard]] inline std::string_view
get_setting_default(std::string_view name) {
    return get_setting_definition(name).default_value;
}

}

#endif
