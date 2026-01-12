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
#include "ores.cli/config/entity_parsers/change_reason_categories_parser.hpp"

#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.cli/config/parser_helpers.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/add_change_reason_category_options.hpp"
#include "ores.database/config/database_configuration.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"

namespace ores::cli::config::entity_parsers {

namespace {

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::command_line_parser;
using boost::program_options::parse_environment;
using boost::program_options::include_positional;
using boost::program_options::collect_unrecognized;

using ores::cli::config::entity;
using ores::cli::config::options;
using ores::cli::config::parser_exception;
using ores::cli::config::parser_helpers::print_help_command;
using ores::cli::config::parser_helpers::add_common_options;
using ores::cli::config::parser_helpers::validate_operation;
using ores::cli::config::parser_helpers::print_entity_help;
using ores::cli::config::parser_helpers::make_export_options_description;
using ores::cli::config::parser_helpers::make_delete_options_description;
using ores::cli::config::parser_helpers::read_export_options;
using ores::cli::config::parser_helpers::read_delete_options;

const std::string list_command_name("list");
const std::string delete_command_name("delete");
const std::string add_command_name("add");

const std::vector<std::string> allowed_operations{
    list_command_name, delete_command_name, add_command_name
};

options_description make_add_change_reason_category_options_description() {
    options_description r("Add Change Reason Category Options");
    r.add_options()
        ("code", value<std::string>(), "Category code (required)")
        ("description", value<std::string>(), "Description (required)")
        ("recorded-by", value<std::string>(), "Username of modifier (required)")
        ("change-commentary", value<std::string>(), "Change commentary");

    return r;
}

add_change_reason_category_options
read_add_change_reason_category_options(const variables_map& vm) {
    add_change_reason_category_options r;

    if (vm.count("code") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --code for add change-reason-category command."));
    }
    r.code = vm["code"].as<std::string>();

    if (vm.count("description") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --description for add change-reason-category command."));
    }
    r.description = vm["description"].as<std::string>();

    if (vm.count("recorded-by") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --recorded-by for add change-reason-category command."));
    }
    r.recorded_by = vm["recorded-by"].as<std::string>();

    if (vm.count("change-commentary") != 0)
        r.change_commentary = vm["change-commentary"].as<std::string>();

    return r;
}

}

std::optional<options>
handle_change_reason_categories_command(bool has_help,
    const parsed_options& po,
    std::ostream& info,
    variables_map& vm) {

    auto o(collect_unrecognized(po.options, include_positional));
    o.erase(o.begin());

    if (has_help && o.empty()) {
        const std::vector<std::pair<std::string, std::string>> operations = {
            {"list", "List change reason categories as JSON or table"},
            {"delete", "Delete a change reason category by code"},
            {"add", "Add a new change reason category"}
        };
        print_entity_help("change-reason-categories",
            "Manage change reason categories", operations, info);
        return {};
    }

    if (o.empty()) {
        BOOST_THROW_EXCEPTION(parser_exception(
            "change-reason-categories command requires an operation (list, delete, add)"));
    }

    const auto operation = o.front();
    o.erase(o.begin());

    validate_operation("change-reason-categories", operation, allowed_operations);

    options r;
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("CLI"));

    if (operation == list_command_name) {
        auto d = add_common_options(make_export_options_description());
        if (has_help) {
            print_help_command("change-reason-categories list", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.exporting = read_export_options(vm, entity::change_reason_categories);
    } else if (operation == delete_command_name) {
        auto d = add_common_options(make_delete_options_description());
        if (has_help) {
            print_help_command("change-reason-categories delete", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.deleting = read_delete_options(vm, entity::change_reason_categories);
    } else if (operation == add_command_name) {
        auto d = add_common_options(make_add_change_reason_category_options_description());
        if (has_help) {
            print_help_command("change-reason-categories add", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.adding = read_add_change_reason_category_options(vm);
    }

    using ores::database::database_configuration;
    using ores::logging::logging_configuration;
    r.database = database_configuration::read_options(vm);
    r.logging = logging_configuration::read_options(vm);

    return r;
}

}
