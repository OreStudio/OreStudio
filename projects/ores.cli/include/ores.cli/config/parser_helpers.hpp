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
#ifndef ORES_CLI_CONFIG_PARSER_HELPERS_HPP
#define ORES_CLI_CONFIG_PARSER_HELPERS_HPP

#include <string>
#include <vector>
#include <iosfwd>
#include <boost/program_options.hpp>

namespace ores::cli::config::parser_helpers {

/**
 * @brief Prints the header of the help text, applicable to all cases.
 */
void print_help_header(std::ostream& s);

/**
 * @brief Prints help text at the command level.
 *
 * @param command_name name of the command to print help for.
 * @param od command options.
 * @param info information stream.
 */
void print_help_command(const std::string& command_name,
    const boost::program_options::options_description& od, std::ostream& info);

/**
 * @brief Adds database and logging options to an options_description.
 */
boost::program_options::options_description
add_common_options(boost::program_options::options_description base);

/**
 * @brief Validates that an operation is in the list of allowed operations.
 */
void validate_operation(const std::string& entity_name,
    const std::string& operation,
    const std::vector<std::string>& allowed_operations);

/**
 * @brief Prints entity-level help showing available operations.
 */
void print_entity_help(const std::string& entity_name,
    const std::string& description,
    const std::vector<std::pair<std::string, std::string>>& operations,
    std::ostream& info);

}

#endif
