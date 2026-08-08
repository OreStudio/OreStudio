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
#ifndef ORES_PLATFORM_ENVIRONMENT_FAKE_ENVIRONMENT_PROVIDER_HPP
#define ORES_PLATFORM_ENVIRONMENT_FAKE_ENVIRONMENT_PROVIDER_HPP

#include "ores.platform/environment/environment_provider.hpp"
#include <string>
#include <unordered_map>

namespace ores::platform::environment {

/**
 * @brief In-memory environment provider for tests.
 *
 * Stores all key-value pairs in an unordered_map. Tests can pre-populate
 * the fake provider with the exact values the parser under test should
 * see, or leave entries absent to simulate an unset variable.
 *
 * Intended for test-only use — there is no .cpp; the inline
 * implementation is trivial.
 */
class fake_environment_provider final : public environment_provider {
public:
    /**
     * @brief Constructs a fake provider pre-populated with the given
     * key-value pairs.
     */
    explicit fake_environment_provider(std::unordered_map<std::string, std::string> values = {})
        : values_(std::move(values)) {}

    std::optional<std::string> get(const std::string& name) const override {
        auto it = values_.find(name);
        if (it == values_.end())
            return {};
        return it->second;
    }

    void set(const std::string& name, const std::string& value) override {
        values_[name] = value;
    }

    void unset(const std::string& name) override {
        values_.erase(name);
    }

    /// The in-memory key-value store. Tests and scoped guards read
    /// this to know which keys to sync to the real environment.
    std::unordered_map<std::string, std::string> values_;
};

}

#endif
