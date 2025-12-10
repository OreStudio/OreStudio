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
#include "ores.cli/config/entity_parsers/feature_flags_parser.hpp"

#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.cli/config/parser_helpers.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"
#include "ores.utility/database/database_configuration.hpp"
#include "ores.utility/log/logging_configuration.hpp"
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
using ores::cli::config::format;
using ores::cli::config::options;
using ores::cli::config::export_options;
using ores::cli::config::parser_exception;
using ores::cli::config::parser_helpers::print_help_command;
using ores::cli::config::parser_helpers::add_common_options;
using ores::cli::config::parser_helpers::validate_operation;
using ores::cli::config::parser_helpers::print_entity_help;

const std::string export_as_of_arg("as-of");
const std::string export_key_arg("key");
const std::string export_all_versions_arg("all-versions");
const std::string export_format_arg("format");
const std::string delete_key_arg("key");

const std::string delete_command_name("delete");
const std::string list_command_name("list");
const std::string add_command_name("add");

const std::vector<std::string> allowed_operations{
    list_command_name, delete_command_name, add_command_name
};

/**
 * @brief Creates the options related to exporting/listing.
 */
options_description make_export_options_description() {
    options_description r("Export");
    r.add_options()
        ("as-of", value<std::string>(),
            "Time point from which to dump data. If not supplied, defaults to latest.")
        ("key", value<std::string>(), "Key to filter data by.")
        ("all-versions", "If supplied, retrieves all versions.")
        ("format", value<std::string>(), "Format to export data in, e.g. xml or json.");

    return r;
}

/**
 * @brief Creates the options related to deleting entities.
 */
options_description make_delete_options_description() {
    options_description r("Delete");
    r.add_options()
        ("key", value<std::string>(), "Key to identify the entity (e.g., account ID or username).");

    return r;
}

/**
 * @brief Creates the options related to adding feature flags.
 */
options_description make_add_feature_flag_options_description() {
    options_description r("Add Feature Flag Options");
    r.add_options()
        ("name",
            value<std::string>(),
            "Feature flag name (required)")
        ("description",
            value<std::string>()->default_value(""),
            "Feature flag description")
        ("enabled",
            value<bool>()->default_value(false),
            "Whether the feature is enabled (default: false)")
        ("modified-by",
            value<std::string>(),
            "Username of modifier (required)");

    return r;
}

/**
 * @brief Reads format from the variables map.
 */
format read_format(const variables_map& vm) {
    if (vm.count(export_format_arg) == 0)
        return format::json;

    const auto s(vm[export_format_arg].as<std::string>());
    auto f = magic_enum::enum_cast<format>(s);
    if (f.has_value())
        return f.value();

    BOOST_THROW_EXCEPTION(
        parser_exception("Invalid or unsupported format: '" + s + "'"));
}

/**
 * @brief Reads the export configuration from the variables map.
 */
export_options read_export_options(const variables_map& vm, entity e) {
    export_options r;

    r.target_entity = e;
    r.target_format = read_format(vm);
    r.all_versions = vm.count(export_all_versions_arg) != 0;

    if (vm.count(export_as_of_arg) != 0)
        r.as_of = vm[export_as_of_arg].as<std::string>();

    if (vm.count(export_key_arg) != 0)
        r.key = vm[export_key_arg].as<std::string>();

    return r;
}

/**
 * @brief Reads the delete configuration from the variables map.
 */
ores::cli::config::delete_options read_delete_options(const variables_map& vm, entity e) {
    ores::cli::config::delete_options r;

    r.target_entity = e;

    if (vm.count(delete_key_arg) == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --key argument for delete command."));
    }
    r.key = vm[delete_key_arg].as<std::string>();

    return r;
}

/**
 * @brief Reads the add configuration from the variables map for feature flags.
 */
ores::cli::config::add_options read_add_feature_flag_options(const variables_map& vm) {
    ores::cli::config::add_options r;

    r.target_entity = entity::feature_flags;

    if (vm.count("modified-by") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --modified-by for add command."));
    }
    r.modified_by = vm["modified-by"].as<std::string>();

    // Feature flag-specific required fields
    if (vm.count("name") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --name for add feature flag command."));
    }
    r.flag_name = vm["name"].as<std::string>();

    // Optional feature flag fields with defaults
    if (vm.count("description") != 0)
        r.description = vm["description"].as<std::string>();
    if (vm.count("enabled") != 0)
        r.enabled = vm["enabled"].as<bool>();

    return r;
}

}

std::optional<options>
handle_feature_flags_command(bool has_help,
    const parsed_options& po,
    std::ostream& info,
    variables_map& vm) {

    // Collect all unrecognized options from the first pass
    auto o(collect_unrecognized(po.options, include_positional));
    o.erase(o.begin()); // Remove command name

    // Show help for feature_flags command if requested with no operation
    if (has_help && o.empty()) {
        const std::vector<std::pair<std::string, std::string>> operations = {
            {"list", "List feature flags as JSON or table (internal formats)"},
            {"delete", "Delete a feature flag by key"},
            {"add", "Add a new feature flag"}
        };
        print_entity_help("feature_flags", "Manage feature flags", operations, info);
        return {};
    }

    if (o.empty()) {
        BOOST_THROW_EXCEPTION(parser_exception(
            "feature_flags command requires an operation (list, delete, add)"));
    }

    const auto operation = o.front();
    o.erase(o.begin()); // Remove operation from args

    // Validate operation
    validate_operation("feature_flags", operation, allowed_operations);

    options r;
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("CLI"));

    if (operation == list_command_name) {
        auto d = add_common_options(make_export_options_description());
        if (has_help) {
            print_help_command("feature_flags list", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.exporting = read_export_options(vm, entity::feature_flags);
    } else if (operation == delete_command_name) {
        auto d = add_common_options(make_delete_options_description());
        if (has_help) {
            print_help_command("feature_flags delete", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.deleting = read_delete_options(vm, entity::feature_flags);
    } else if (operation == add_command_name) {
        auto d = add_common_options(make_add_feature_flag_options_description());
        if (has_help) {
            print_help_command("feature_flags add", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.adding = read_add_feature_flag_options(vm);
    }

    // Read common options
    using ores::utility::database::database_configuration;
    using ores::utility::log::logging_configuration;
    r.database = database_configuration::read_options(vm);
    r.logging = logging_configuration::read_options(vm);

    return r;
}

}
