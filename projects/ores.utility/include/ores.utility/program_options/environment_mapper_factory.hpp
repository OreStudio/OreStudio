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
#ifndef ORES_UTILITY_PROGRAM_OPTIONS_ENVIRONMENT_MAPPER_HPP
#define ORES_UTILITY_PROGRAM_OPTIONS_ENVIRONMENT_MAPPER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <functional>

namespace ores::utility::program_options {

/**
 * @brief Makes environment mappers for program options.
 *
 * @note Logging is not available during configuration.
 */
class environment_mapper_factory {
public:
    /**
     * @brief Creates an environment variable name mapping function.
     *
     * Maps environment variables with ORES_ prefix to command-line option
     * names. For example: ORES_DB_PASSWORD -> db-password
     *
     * @param app_name name of the app, in capitals, e.g. CLI.
     *
     * @return Function that maps environment variable names to option names.
     */
    static std::function<std::string(const std::string&)>
    make_mapper(const std::string app_name);
};

}

#endif
