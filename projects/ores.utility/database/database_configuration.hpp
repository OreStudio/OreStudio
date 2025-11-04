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
#ifndef ORES_UTILITY_DATABASE_DATABASE_CONFIGURATION_HPP
#define ORES_UTILITY_DATABASE_DATABASE_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <optional>
#include <functional>
#include <boost/program_options.hpp>
#include "ores.utility/database/database_options.hpp"

namespace ores::utility::database {

/**
 * @brief Centralized manager for database configuration parsing.
 *
 * Provides utilities for creating command-line option descriptions,
 * reading configuration from parsed options, and mapping environment
 * variables.
 */
class database_configuration final {
public:
    database_configuration() = delete;

    /**
     * @brief Creates the boost::program_options description for database CLI
     * arguments.
     *
     * @return options_description for database configuration.
     */
    static boost::program_options::options_description
    make_options_description();

    /**
     * @brief Reads database options from parsed variables map.
     *
     * If no database options are present in the variables map, returns
     * std::nullopt. If partial options are present, uses sensible defaults
     * for missing values. Password is required if any database option is
     * present.
     *
     * @param vm Parsed command-line options.
     * @return database_options if any database configuration was provided,
     * std::nullopt otherwise.
     */
    static std::optional<database_options>
    read_options(const boost::program_options::variables_map& vm);

    /**
     * @brief Creates an environment variable name mapping function.
     *
     * Maps environment variables with ORES_ prefix to command-line option
     * names. For example: ORES_DB_PASSWORD -> db-password
     *
     * @return Function that maps environment variable names to option names.
     */
    static std::function<std::string(const std::string&)>
    make_environment_mapper();
};

}

#endif
