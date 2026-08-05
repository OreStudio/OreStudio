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
#ifndef ORES_TESTING_SCOPED_ENV_UNSET_HPP
#define ORES_TESTING_SCOPED_ENV_UNSET_HPP

#include "ores.platform/environment/environment.hpp"
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace ores::testing {

/**
 * @brief Unsets a list of environment variables for the scope's lifetime,
 * restoring their prior values (if any) on destruction.
 *
 * Local (non-CI) ctest runs load the whole developer .env into every test
 * binary's environment (see the top-level CMakeLists.txt), including
 * fleet-wide vars like ORES_NATS_URL. Tests asserting a parser's
 * compiled-in defaults with empty argv need this to stay hermetic
 * regardless of what the developer's .env happens to contain -- CI itself
 * is unaffected (it does not load .env), but a correct test should not
 * depend on that distinction.
 */
class scoped_env_unset final {
public:
    explicit scoped_env_unset(std::vector<std::string> names)
        : names_(std::move(names)) {
        using ores::platform::environment::environment;
        for (const auto& name : names_) {
            previous_.emplace_back(name, environment::get_value(name));
            environment::unset_value(name);
        }
    }

    scoped_env_unset(const scoped_env_unset&) = delete;
    scoped_env_unset& operator=(const scoped_env_unset&) = delete;

    ~scoped_env_unset() {
        using ores::platform::environment::environment;
        for (const auto& [name, value] : previous_) {
            if (value)
                environment::set_value(name, *value);
            else
                environment::unset_value(name);
        }
    }

private:
    std::vector<std::string> names_;
    std::vector<std::pair<std::string, std::optional<std::string>>> previous_;
};

}

#endif
