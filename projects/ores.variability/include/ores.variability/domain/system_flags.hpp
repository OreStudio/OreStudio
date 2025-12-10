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
#ifndef ORES_VARIABILITY_DOMAIN_SYSTEM_FLAGS_HPP
#define ORES_VARIABILITY_DOMAIN_SYSTEM_FLAGS_HPP

#include <array>
#include <string>
#include <ostream>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <magic_enum/magic_enum.hpp>

namespace ores::variability::domain {

/**
 * @brief Enumeration of well-known system flags.
 *
 * System flags are compile-time known feature flags that control core system
 * behaviour. Unlike dynamic feature flags which can be created at runtime,
 * system flags have predefined names, descriptions, and default values.
 *
 * The flag names in the database use the "system." prefix followed by the
 * snake_case enum name (e.g., system_flag::bootstrap_mode maps to
 * "system.bootstrap_mode").
 */
enum class system_flag {
    /**
     * @brief Indicates whether the system is in bootstrap mode.
     *
     * When enabled, the system is waiting for the initial admin account to be
     * created. Once an admin account exists, this flag should be disabled.
     * Default: enabled (true)
     */
    bootstrap_mode,

    /**
     * @brief Controls whether user self-registration is allowed.
     *
     * When enabled, users can create their own accounts through the signup
     * process. When disabled, only administrators can create accounts.
     * Default: disabled (false)
     */
    user_signups
};

/**
 * @brief Metadata for a system flag including its default state and description.
 */
struct system_flag_definition {
    system_flag flag;
    bool default_enabled;
    std::string_view description;
};

/**
 * @brief Compile-time manifest of all system flags with their defaults.
 *
 * This array defines all well-known system flags, their default enabled state,
 * and human-readable descriptions. The flag_initializer service uses this
 * manifest to ensure all system flags exist in the database at startup.
 */
inline constexpr std::array system_flag_definitions = {
    system_flag_definition {
        .flag = system_flag::bootstrap_mode,
        .default_enabled = true,
        .description = "Indicates whether the system is in bootstrap mode "
        "(waiting for initial admin account)."
    },
    system_flag_definition {
        .flag = system_flag::user_signups,
        .default_enabled = false,
        .description = "Controls whether user self-registration is allowed."
    }
};

/**
 * @brief Converts a system_flag enum value to its database name.
 *
 * The database name uses the "system." prefix followed by the snake_case
 * enum name.
 *
 * @param flag The system flag enum value.
 * @return The database name string (e.g., "system.bootstrap_mode").
 */
[[nodiscard]] inline std::string to_flag_name(system_flag flag) {
    return std::string("system.") + std::string(magic_enum::enum_name(flag));
}

/**
 * @brief Attempts to parse a database flag name to a system_flag enum.
 *
 * @param name The database name (e.g., "system.bootstrap_mode").
 * @return The system_flag if the name matches a known flag, std::nullopt otherwise.
 */
[[nodiscard]] inline std::optional<system_flag> from_flag_name(std::string_view name) {
    constexpr std::string_view prefix = "system.";
    if (!name.starts_with(prefix)) {
        return std::nullopt;
    }

    auto enum_name = name.substr(prefix.size());
    return magic_enum::enum_cast<system_flag>(enum_name);
}

/**
 * @brief Gets the definition for a system flag.
 *
 * @param flag The system flag to look up.
 * @return The flag definition containing default state and description.
 */
[[nodiscard]] inline const system_flag_definition&
get_definition(system_flag flag) {
    for (const auto& def : system_flag_definitions) {
        if (def.flag == flag)
            return def;
    }
    // This should never happen for valid enum values.
    throw std::logic_error("Definition for system_flag not found.");
}

/**
 * @brief Stream output operator for system_flag.
 *
 * Outputs the flag name in a human-readable format.
 */
inline std::ostream& operator<<(std::ostream& os, system_flag flag) {
    return os << to_flag_name(flag);
}

}

#endif
