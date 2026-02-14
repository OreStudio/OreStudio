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
#ifndef ORES_UTILITY_GENERATION_GENERATION_ENVIRONMENT_HPP
#define ORES_UTILITY_GENERATION_GENERATION_ENVIRONMENT_HPP

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

namespace ores::utility::generation {

/**
 * @brief Scoped key-value store for generation context data.
 *
 * Supports parent-child chaining: a child environment inherits all entries
 * from its parent, with overrides taking precedence.
 */
class generation_environment final {
public:
    using entries = std::unordered_map<std::string, std::string>;

    /**
     * @brief Constructs a root environment with optional initial entries.
     */
    explicit generation_environment(entries initial = {});

    /**
     * @brief Constructs a child environment with overrides.
     */
    generation_environment(
        std::shared_ptr<const generation_environment> parent,
        entries overrides);

    /**
     * @brief Gets a value by key, searching parent chain.
     */
    std::optional<std::string> get(std::string_view key) const;

    /**
     * @brief Gets a value by key, returning default if not found.
     */
    std::string get_or(std::string_view key,
        const std::string& default_value) const;

    /**
     * @brief Checks if a key exists in this environment or parents.
     */
    bool has(std::string_view key) const;

    /**
     * @brief Returns the parent environment, if any.
     */
    std::shared_ptr<const generation_environment> parent() const {
        return parent_;
    }

private:
    std::shared_ptr<const generation_environment> parent_;
    entries entries_;
};

}

#endif
