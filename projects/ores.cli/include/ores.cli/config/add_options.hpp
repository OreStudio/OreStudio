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
#ifndef ORES_CLI_CONFIG_ADD_OPTIONS_HPP
#define ORES_CLI_CONFIG_ADD_OPTIONS_HPP

#include <string>
#include <optional>
#include "ores.cli/config/entity.hpp"

namespace ores::cli::config {

/**
 * @brief Configuration for adding entities to the system via command-line arguments.
 */
struct add_options final {
    entity target_entity;

    // Currency-specific fields
    std::optional<std::string> iso_code;
    std::optional<std::string> name;
    std::optional<std::string> numeric_code;
    std::optional<std::string> symbol;
    std::optional<std::string> fraction_symbol;
    std::optional<int> fractions_per_unit;
    std::optional<std::string> rounding_type;
    std::optional<int> rounding_precision;
    std::optional<std::string> format;
    std::optional<std::string> currency_type;
    std::optional<std::string> modified_by;

    // Account-specific fields
    std::optional<std::string> username;
    std::optional<std::string> email;
    std::optional<std::string> password;
    std::optional<bool> is_admin;

    // Feature flag-specific fields
    std::optional<std::string> flag_name;
    std::optional<std::string> description;
    std::optional<bool> enabled;
};

}

#endif
