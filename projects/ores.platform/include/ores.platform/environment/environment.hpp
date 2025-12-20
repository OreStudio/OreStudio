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
#ifndef ORES_PLATFORM_ENVIRONMENT_ENVIRONMENT_HPP
#define ORES_PLATFORM_ENVIRONMENT_ENVIRONMENT_HPP

#include <string>
#include <optional>

namespace ores::platform::environment {

/**
 * @brief Utilities for reading environment variables.
 */
class environment final {
public:
    /**
     * @brief Gets an environment variable value.
     *
     * @param name Name of the environment variable.
     * @return The value if found, empty optional otherwise.
     */
    static std::optional<std::string> get_value(const std::string& name);

    /**
     * @brief Gets an environment variable value with a default.
     *
     * @param name Name of the environment variable.
     * @param default_value Default value if not found.
     * @return The value if found, default_value otherwise.
     */
    static std::string get_value_or_default(const std::string& name,
        const std::string& default_value);

    /**
     * @brief Gets an environment variable value as an integer.
     *
     * @param name Name of the environment variable.
     * @param default_value Default value if not found or not parseable.
     * @return The value if found and parseable, default_value otherwise.
     */
    static int get_int_value_or_default(const std::string& name,
        int default_value);

    /**
     * @brief Sets an environment variable value.
     *
     * @param name Name of the environment variable.
     * @param value Value to set.
     */
    static void set_value(const std::string& name, const std::string& value);
};

}

#endif
