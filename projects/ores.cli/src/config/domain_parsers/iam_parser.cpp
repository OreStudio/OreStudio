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
#include "ores.cli/config/domain_parsers/iam_parser.hpp"
#include "ores.cli/config/entity_parsers/accounts_parser.hpp"
#include "ores.cli/config/entity_parsers/login_info_parser.hpp"
#include "ores.cli/config/entity_parsers/permissions_parser.hpp"
#include "ores.cli/config/entity_parsers/roles_parser.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
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

        const std::string accounts_command_name("accounts");
        const std::string accounts_command_desc("Manage accounts (list, delete, add).");

        const std::string roles_command_name("roles");
        const std::string roles_command_desc("Manage roles (list, delete).");

        const std::string permissions_command_name("permissions");
        const std::string permissions_command_desc("Manage permissions (list, delete).");

        const std::string login_info_command_name("login-info");
        const std::string login_info_command_desc("View login tracking information (list).");

        const std::string domain_name("iam");
        const std::string domain_desc("iam: Identity and access management.");

        void print_domain_help(std::ostream& info) {
            info << domain_desc << std::endl << std::endl;

            const unsigned int command_width(15);
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << accounts_command_name << accounts_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << roles_command_name << roles_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << permissions_command_name << permissions_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << login_info_command_name << login_info_command_desc << std::endl;

            info << std::endl
                 << "Use: ores.cli iam <entity> <operation> --help for details." << std::endl;
        }

        void validate_entity_name(const std::string& entity_name) {
            const bool is_valid(
                entity_name == accounts_command_name || entity_name == roles_command_name ||
                entity_name == permissions_command_name || entity_name == login_info_command_name);

            if (!is_valid) {
                BOOST_THROW_EXCEPTION(parser_exception(
                    std::format("Invalid or unsupported entity: {}. "
                                "Available entities: accounts, roles, permissions, login-info",
                                entity_name)));
            }
        }

    }

    std::optional<options> handle_iam_command(bool has_help,
                                              const parsed_options& po,
                                              std::ostream& info,
                                              variables_map& vm) {

        auto o(collect_unrecognized(po.options, include_positional));

        if (has_help && o.empty()) {
            print_domain_help(info);
            return {};
        }

        if (o.empty()) {
            BOOST_THROW_EXCEPTION(parser_exception(
                "iam domain requires an entity (accounts, roles, permissions, login-info)"));
        }

        const auto entity_name = o.front();
        o.erase(o.begin());

        validate_entity_name(entity_name);

        if (entity_name == accounts_command_name) {
            return entity_parsers::handle_accounts_command(has_help, po, info, vm);
        } else if (entity_name == roles_command_name) {
            return entity_parsers::handle_roles_command(has_help, po, info, vm);
        } else if (entity_name == permissions_command_name) {
            return entity_parsers::handle_permissions_command(has_help, po, info, vm);
        } else if (entity_name == login_info_command_name) {
            return entity_parsers::handle_login_info_command(has_help, po, info, vm);
        }

        return {};
    }

}
