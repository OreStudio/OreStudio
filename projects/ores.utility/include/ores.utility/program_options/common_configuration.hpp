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
#ifndef ORES_UTILITY_PROGRAM_OPTIONS_COMMON_CONFIGURATION_HPP
#define ORES_UTILITY_PROGRAM_OPTIONS_COMMON_CONFIGURATION_HPP

#include <boost/program_options.hpp>

namespace ores::utility::program_options {

/**
 * @brief Common options shared by all applications.
 */
struct common_options {
    bool verbose = false;
};

/**
 * @brief Provides common command-line options for all applications.
 *
 * This configuration provides the standard options that every application
 * should support: help, version, and verbose output.
 *
 * Standard options:
 *   -h, --help     Display help and exit
 *   -v, --version  Display version and exit
 *       --verbose  Enable verbose output
 */
class common_configuration final {
public:
    common_configuration() = delete;

    /**
     * @brief Creates the options description for common CLI arguments.
     *
     * Note: Help and version are typically handled by the application's
     * main parser before other options are processed. This method provides
     * them for consistency and documentation purposes.
     *
     * @return options_description containing help, version, and verbose.
     */
    static boost::program_options::options_description
    make_options_description();

    /**
     * @brief Reads common options from parsed variables map.
     *
     * @param vm Parsed command-line options.
     * @return common_options with verbose flag set appropriately.
     */
    static common_options
    read_options(const boost::program_options::variables_map& vm);
};

}

#endif
