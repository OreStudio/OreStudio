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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_TOPOLOGY_BUILD_ERROR_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_TOPOLOGY_BUILD_ERROR_HPP

#include "ores.analytics.quant/domain/topology_error.hpp"
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::analytics::quant::domain {

/**
 * @brief Thrown by @c topology_builder::build when the input cannot form a
 * valid spanning tree.
 *
 * Carries every violation found across the whole input (not just the
 * first), each with a readable @c topology_error::describe(). A config
 * that admits more than one path between two currencies is always
 * rejected here -- @c topology_builder never silently picks one of
 * several possible paths.
 */
class topology_build_error : public std::runtime_error {
public:
    explicit topology_build_error(std::vector<topology_error> errors)
        : std::runtime_error(build_message(errors))
        , errors_(std::move(errors)) {}

    [[nodiscard]] const std::vector<topology_error>& errors() const noexcept {
        return errors_;
    }

private:
    static std::string build_message(const std::vector<topology_error>& errors) {
        std::string message =
            "CRM topology build failed with " + std::to_string(errors.size()) + " error(s):";
        for (const auto& error : errors) {
            message += "\n  - " + error.describe();
        }
        return message;
    }

    std::vector<topology_error> errors_;
};

} // namespace ores::analytics::quant::domain

#endif
