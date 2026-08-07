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
#ifndef ORES_TESTING_SCOPED_ENVIRONMENT_OVERRIDE_HPP
#define ORES_TESTING_SCOPED_ENVIRONMENT_OVERRIDE_HPP

#include "ores.platform/environment/environment.hpp"
#include "ores.platform/environment/fake_environment_provider.hpp"
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::testing {

/**
 * @brief RAII guard that swaps in a fake environment provider and
 * syncs the real environment for boost::program_options compatibility.
 *
 * boost::program_options reads env vars from the real process
 * environment. This guard saves real values for every overridden or
 * stripped key, swaps in the fake provider, and syncs the real env.
 * On destruction everything is restored.
 */
class scoped_environment_override final {
public:
    /**
     * @brief Constructs with overridden values and optional stripped keys.
     *
     * @param values key-value pairs to set (both on the fake provider
     *   and the real environment).
     * @param stripped_keys keys to remove from the real environment
     *   (and set as absent on the fake provider). Use this for vars
     *   you want the parser NOT to see — they are saved and restored
     *   on destruction.
     */
    explicit scoped_environment_override(
        std::unordered_map<std::string, std::string> values = {},
        std::vector<std::string> stripped_keys = {})
        : fake_(std::move(values)),
          stripped_(std::move(stripped_keys)) {
        using ores::platform::environment::environment;

        // Save and unset stripped keys.
        for (const auto& name : stripped_) {
            saved_[name] = environment::get_value(name);
            environment::unset_value(name);
        }

        // Save and override values.
        for (const auto& [name, value] : fake_.values_) {
            if (!saved_.contains(name))
                saved_[name] = environment::get_value(name);
            environment::set_value(name, value);
        }

        previous_ = environment::swap_provider(&fake_);
    }

    scoped_environment_override(const scoped_environment_override&) = delete;
    scoped_environment_override& operator=(const scoped_environment_override&) = delete;

    ~scoped_environment_override() {
        using ores::platform::environment::environment;

        environment::swap_provider(previous_);

        // Restore saved values.
        for (const auto& [name, value] : saved_) {
            if (value)
                environment::set_value(name, *value);
            else
                environment::unset_value(name);
        }
    }

    ores::platform::environment::fake_environment_provider& fake() {
        return fake_;
    }

private:
    ores::platform::environment::fake_environment_provider fake_;
    ores::platform::environment::environment_provider* previous_;
    std::vector<std::string> stripped_;
    std::unordered_map<std::string, std::optional<std::string>> saved_;
};

}

#endif
