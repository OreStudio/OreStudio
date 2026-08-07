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
#ifndef ORES_PLATFORM_ENVIRONMENT_ENVIRONMENT_PROVIDER_HPP
#define ORES_PLATFORM_ENVIRONMENT_ENVIRONMENT_PROVIDER_HPP

#include <optional>
#include <string>

namespace ores::platform::environment {

/**
 * @brief Abstract interface for environment variable access.
 *
 * Production code uses the real (process-backed) implementation; tests
 * can inject a fake implementation with an in-memory map for isolated,
 * parallel-safe control over what environment variables a parser under
 * test sees.
 */
class environment_provider {
public:
    virtual ~environment_provider() = default;

    /**
     * @brief Gets an environment variable value.
     */
    virtual std::optional<std::string> get(const std::string& name) const = 0;

    /**
     * @brief Sets an environment variable value.
     */
    virtual void set(const std::string& name, const std::string& value) = 0;

    /**
     * @brief Removes an environment variable.
     */
    virtual void unset(const std::string& name) = 0;
};

}

#endif
