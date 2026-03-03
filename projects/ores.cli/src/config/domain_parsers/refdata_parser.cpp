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
#include "ores.cli/config/domain_parsers/refdata_parser.hpp"
#include "ores.cli/config/entity_parsers/countries_parser.hpp"
#include "ores.cli/config/entity_parsers/currencies_parser.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <algorithm>
#include <format>
#include <iomanip>

namespace ores::cli::config::domain_parsers {

    namespace {

        using boost::program_options::variables_map;
        using boost::program_options::parsed_options;
        using boost::program_options::collect_unrecognized;
        using boost::program_options::include_positional;

        using ores::cli::config::options;
        using ores::cli::config::parser_exception;
        namespace entity_parsers = ores::cli::config::entity_parsers;

        const std::string currencies_command_name("currencies");
        const std::string
            currencies_command_desc("Manage currencies (import, export, list, delete, add).");

        const std::string countries_command_name("countries");
        const std::string countries_command_desc("Manage countries (list, delete, add).");

        const std::string domain_name("refdata");
        const std::string domain_desc("refdata: Reference data management.");

        void print_domain_help(std::ostream& info) {
            info << domain_desc << std::endl << std::endl;

            const unsigned int command_width(15);
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << currencies_command_name << currencies_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << countries_command_name << countries_command_desc << std::endl;

            info << std::endl
                 << "Use: ores.cli refdata <entity> <operation> --help for details." << std::endl;
        }

        void validate_entity_name(const std::string& entity_name) {
            const bool is_valid(entity_name == currencies_command_name ||
                                entity_name == countries_command_name);

            if (!is_valid) {
                BOOST_THROW_EXCEPTION(
                    parser_exception(std::format("Invalid or unsupported entity: {}. "
                                                 "Available entities: currencies, countries",
                                                 entity_name)));
            }
        }

    }

    std::optional<options> handle_refdata_command(bool has_help,
                                                  const parsed_options& po,
                                                  std::ostream& info,
                                                  variables_map& vm) {

        const std::string entity_name = vm["command"].as<std::string>();

        // Get remaining args from po - skip domain + entity
        // po.unrecognized contains all positional args not consumed by top-level parser
        auto unrecognized = collect_unrecognized(po.options, include_positional);

        // Skip domain + entity - rest is operation + options
        // unrecognized = [domain, entity, operation, options...]
        std::vector<std::string> operation_and_options;
        if (unrecognized.size() > 2) {
            operation_and_options =
                std::vector<std::string>(unrecognized.begin() + 2, unrecognized.end());
        }

        if (has_help && operation_and_options.empty()) {
            print_domain_help(info);
            return {};
        }

        // Remove --help from operation_and_options if present (has_help is already set)
        if (has_help) {
            operation_and_options.erase(
                std::remove(operation_and_options.begin(), operation_and_options.end(), "--help"),
                operation_and_options.end());
        }

        validate_entity_name(entity_name);

        // Prepend entity name so entity parser can erase it (it expects first element = entity)
        std::vector<std::string> entity_args_with_name;
        entity_args_with_name.push_back(entity_name);
        entity_args_with_name.insert(entity_args_with_name.end(), operation_and_options.begin(),
                                     operation_and_options.end());

        boost::program_options::options_description desc;
        desc.add_options()("command", boost::program_options::value<std::string>(), "Command");

        boost::program_options::positional_options_description pos;
        pos.add("command", -1);

        auto new_po = boost::program_options::command_line_parser(entity_args_with_name)
                          .options(desc)
                          .positional(pos)
                          .allow_unregistered()
                          .run();

        if (entity_name == currencies_command_name) {
            return entity_parsers::handle_currencies_command(has_help, new_po, info, vm);
        } else if (entity_name == countries_command_name) {
            return entity_parsers::handle_countries_command(has_help, new_po, info, vm);
        }

        return {};
    }

}
