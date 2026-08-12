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
#ifndef ORES_PLATFORM_ENVIRONMENT_REAL_ENVIRONMENT_PROVIDER_HPP
#define ORES_PLATFORM_ENVIRONMENT_REAL_ENVIRONMENT_PROVIDER_HPP

#include "ores.platform/environment/environment_provider.hpp"

namespace ores::platform::environment {

/**
 * @brief Production environment provider backed by the real process
 * environment.
 *
 * Wraps std::getenv / setenv / unsetenv (Unix) and _putenv_s (Windows).
 * This is the provider that ores.platform uses by default. Callers must
 * go through this provider: std::getenv is deprecated in MSVC's STL
 * (in favour of _dupenv_s) and breaks the Windows build under -Werror.
 */
class real_environment_provider final : public environment_provider {
public:
    std::optional<std::string> get(const std::string& name) const override;
    void set(const std::string& name, const std::string& value) override;
    void unset(const std::string& name) override;
};

}

#endif
