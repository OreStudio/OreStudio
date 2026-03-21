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
#include "ores.cli/config/domain_parsers/compute_parser.hpp"
#include "ores.cli/config/entity_parsers/compute_hosts_parser.hpp"
#include "ores.cli/config/entity_parsers/compute_apps_parser.hpp"
#include "ores.cli/config/entity_parsers/compute_app_versions_parser.hpp"
#include "ores.cli/config/entity_parsers/compute_batches_parser.hpp"
#include "ores.cli/config/entity_parsers/compute_workunits_parser.hpp"
#include "ores.cli/config/entity_parsers/compute_results_parser.hpp"
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

        const std::string hosts_command_name("hosts");
        const std::string hosts_command_desc("Manage compute hosts (list).");

        const std::string apps_command_name("apps");
        const std::string apps_command_desc("Manage compute apps (list, add).");

        const std::string app_versions_command_name("app-versions");
        const std::string app_versions_command_desc("Manage compute app versions (list, add).");

        const std::string batches_command_name("batches");
        const std::string batches_command_desc("Manage compute batches (list, add).");

        const std::string workunits_command_name("workunits");
        const std::string workunits_command_desc("Manage compute workunits (list).");

        const std::string results_command_name("results");
        const std::string results_command_desc("Manage compute results (list).");

        const std::string domain_name("compute");
        const std::string domain_desc("compute: BOINC-inspired distributed compute grid.");

        void print_domain_help(std::ostream& info) {
            info << domain_desc << std::endl << std::endl;

            const unsigned int command_width(20);
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << hosts_command_name << hosts_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << apps_command_name << apps_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << app_versions_command_name << app_versions_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << batches_command_name << batches_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << workunits_command_name << workunits_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << results_command_name << results_command_desc << std::endl;

            info << std::endl
                 << "Use: ores.cli compute <entity> <operation> --help for details."
                 << std::endl;
        }

        void validate_entity_name(const std::string& entity_name) {
            const bool is_valid(
                entity_name == hosts_command_name ||
                entity_name == apps_command_name ||
                entity_name == app_versions_command_name ||
                entity_name == batches_command_name ||
                entity_name == workunits_command_name ||
                entity_name == results_command_name);

            if (!is_valid) {
                BOOST_THROW_EXCEPTION(
                    parser_exception(std::format("Invalid or unsupported entity: {}. "
                                                 "Available entities: hosts, apps, app-versions, "
                                                 "batches, workunits, results",
                                                 entity_name)));
            }
        }

    }

    std::optional<options> handle_compute_command(bool has_help,
                                                  const parsed_options& po,
                                                  std::ostream& info,
                                                  variables_map& vm) {

        const std::string entity_name = vm["command"].as<std::string>();

        auto unrecognized = collect_unrecognized(po.options, include_positional);

        std::vector<std::string> operation_and_options;
        if (unrecognized.size() > 2) {
            operation_and_options =
                std::vector<std::string>(unrecognized.begin() + 2, unrecognized.end());
        }

        if (has_help && operation_and_options.empty()) {
            print_domain_help(info);
            return {};
        }

        if (has_help) {
            operation_and_options.erase(
                std::remove(operation_and_options.begin(), operation_and_options.end(), "--help"),
                operation_and_options.end());
        }

        validate_entity_name(entity_name);

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

        if (entity_name == hosts_command_name) {
            return entity_parsers::handle_compute_hosts_command(has_help, new_po, info, vm);
        } else if (entity_name == apps_command_name) {
            return entity_parsers::handle_compute_apps_command(has_help, new_po, info, vm);
        } else if (entity_name == app_versions_command_name) {
            return entity_parsers::handle_compute_app_versions_command(has_help, new_po, info, vm);
        } else if (entity_name == batches_command_name) {
            return entity_parsers::handle_compute_batches_command(has_help, new_po, info, vm);
        } else if (entity_name == workunits_command_name) {
            return entity_parsers::handle_compute_workunits_command(has_help, new_po, info, vm);
        } else if (entity_name == results_command_name) {
            return entity_parsers::handle_compute_results_command(has_help, new_po, info, vm);
        }

        return {};
    }

}
