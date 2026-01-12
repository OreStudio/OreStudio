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
#include <optional>
#include <iosfwd>
#include <boost/program_options.hpp>
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"
#include "ores.cli/config/options.hpp"
#include "ores.cli/config/export_options.hpp"
#include "ores.cli/config/delete_options.hpp"

namespace ores::cli::config::parser_helpers {

/**
 * @brief Configuration for a simple entity that supports only list and delete.
 */
struct simple_entity_config {
    std::string name;
    std::string description;
    entity entity_value;
    std::string list_description;
    std::string delete_description;
};

/**
 * @brief Generic handler for entities with only list and delete operations.
 *
 * This eliminates boilerplate code for simple entity parsers.
 */
std::optional<options>
handle_simple_entity_command(
    const simple_entity_config& cfg,
    bool has_help,
    const boost::program_options::parsed_options& po,
    std::ostream& info,
    boost::program_options::variables_map& vm);

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

/**
 * @brief Creates the options related to exporting/listing.
 */
boost::program_options::options_description make_export_options_description();

/**
 * @brief Creates the options related to deleting entities.
 */
boost::program_options::options_description make_delete_options_description();

/**
 * @brief Reads format from the variables map.
 */
format read_format(const boost::program_options::variables_map& vm);

/**
 * @brief Reads the export configuration from the variables map.
 */
export_options read_export_options(
    const boost::program_options::variables_map& vm, entity e);

/**
 * @brief Reads the delete configuration from the variables map.
 */
delete_options read_delete_options(
    const boost::program_options::variables_map& vm, entity e);

}

#endif
