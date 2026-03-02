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
#include "ores.cli/config/domain_parsers/dq_parser.hpp"
#include "ores.cli/config/entity_parsers/change_reason_categories_parser.hpp"
#include "ores.cli/config/entity_parsers/change_reasons_parser.hpp"
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

        const std::string change_reasons_command_name("change-reasons");
        const std::string change_reasons_command_desc("Manage change reasons (list, delete).");

        const std::string change_reason_categories_command_name("change-reason-categories");
        const std::string change_reason_categories_command_desc(
            "Manage change reason categories (list, delete).");

        const std::string domain_name("dq");
        const std::string domain_desc("dq: Data quality.");

        void print_domain_help(std::ostream& info) {
            info << domain_desc << std::endl << std::endl;

            const unsigned int command_width(25);
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << change_reasons_command_name << change_reasons_command_desc << std::endl;
            info << "  " << std::setfill(' ') << std::left << std::setw(command_width)
                 << change_reason_categories_command_name << change_reason_categories_command_desc
                 << std::endl;

            info << std::endl
                 << "Use: ores.cli dq <entity> <operation> --help for details." << std::endl;
        }

        void validate_entity_name(const std::string& entity_name) {
            const bool is_valid(entity_name == change_reasons_command_name ||
                                entity_name == change_reason_categories_command_name);

            if (!is_valid) {
                BOOST_THROW_EXCEPTION(parser_exception(
                    std::format("Invalid or unsupported entity: {}. "
                                "Available entities: change-reasons, change-reason-categories",
                                entity_name)));
            }
        }

    }

    std::optional<options> handle_dq_command(bool has_help,
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
                "dq domain requires an entity (change-reasons, change-reason-categories)"));
        }

        const auto entity_name = o.front();
        o.erase(o.begin());

        validate_entity_name(entity_name);

        if (entity_name == change_reasons_command_name) {
            return entity_parsers::handle_change_reasons_command(has_help, po, info, vm);
        } else if (entity_name == change_reason_categories_command_name) {
            return entity_parsers::handle_change_reason_categories_command(has_help, po, info, vm);
        }

        return {};
    }

}
