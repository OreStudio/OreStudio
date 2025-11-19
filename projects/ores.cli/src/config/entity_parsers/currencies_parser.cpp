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
#include "ores.cli/config/entity_parsers/currencies_parser.hpp"

#include <filesystem>
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
using ores::cli::config::import_options;
using ores::cli::config::export_options;
using ores::cli::config::parser_exception;
using ores::cli::config::parser_helpers::print_help_command;
using ores::cli::config::parser_helpers::add_common_options;
using ores::cli::config::parser_helpers::force_entity;
using ores::cli::config::parser_helpers::validate_operation;
using ores::cli::config::parser_helpers::print_entity_help;

const std::string entity_arg("entity");
const std::string import_targets_arg("target");
const std::string export_as_of_arg("as-of");
const std::string export_key_arg("key");
const std::string export_all_versions_arg("all-versions");
const std::string export_format_arg("format");
const std::string delete_key_arg("key");

const std::string import_command_name("import");
const std::string export_command_name("export");
const std::string delete_command_name("delete");
const std::string list_command_name("list");
const std::string add_command_name("add");

const std::vector<std::string> allowed_operations{
    import_command_name, export_command_name, list_command_name,
    delete_command_name, add_command_name
};

/**
 * @brief Creates the options related to importing.
 */
options_description make_import_options_description() {
    options_description r("Import");
    r.add_options()
        ("entity",
            value<std::string>(),
            "Entity to import, e.g. 'currency_config', etc.")
        ("target",
            value<std::vector<std::string>>(),
            "One or more target files containing entities.");

    return r;
}

/**
 * @brief Creates the options related to ORE exporting.
 */
options_description make_export_options_description() {
    options_description r("Export");
    r.add_options()
        ("entity",
            value<std::string>(),
            "Entity to export, e.g. 'currency_config', etc.")
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
        ("entity",
            value<std::string>(),
            "Entity to delete, e.g. 'accounts', etc.")
        ("key", value<std::string>(), "Key to identify the entity (e.g., account ID or username).");

    return r;
}

/**
 * @brief Creates the options related to adding currencies.
 */
options_description make_add_currency_options_description() {
    options_description r("Add Currency Options");
    r.add_options()
        ("iso-code",
            value<std::string>(),
            "Currency ISO code (required, e.g., USD)")
        ("name",
            value<std::string>(),
            "Currency name (required, e.g., 'United States Dollar')")
        ("numeric-code",
            value<std::string>()->default_value("0"),
            "Currency numeric code")
        ("symbol",
            value<std::string>()->default_value(""),
            "Currency symbol")
        ("fraction-symbol",
            value<std::string>()->default_value(""),
            "Fraction symbol")
        ("fractions-per-unit",
            value<int>()->default_value(100),
            "Fractions per unit")
        ("rounding-type",
            value<std::string>()->default_value("Closest"),
            "Rounding type")
        ("rounding-precision",
            value<int>()->default_value(2),
            "Rounding precision")
        ("format",
            value<std::string>()->default_value(""),
            "Display format")
        ("currency-type",
            value<std::string>()->default_value(""),
            "Currency type")
        ("modified-by",
            value<std::string>(),
            "Username of modifier (required)");

    return r;
}

/**
 * @brief Reads entity from the variables map.
 */
entity read_entity(const variables_map& vm) {
    if (vm.count(entity_arg) == 0)
        BOOST_THROW_EXCEPTION(parser_exception("Must supply entity."));

    const auto s(vm[entity_arg].as<std::string>());
    auto e = magic_enum::enum_cast<entity>(s);
    if (e.has_value())
        return e.value();

    BOOST_THROW_EXCEPTION(
        parser_exception("Invalid or unsupported entity: '" + s + "'"));
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
 * @brief Reads the import configuration from the variables map.
 */
import_options read_import_options(const variables_map& vm) {
    import_options r;

    r.target_entity = read_entity(vm);

    const auto t(vm[import_targets_arg].as<std::vector<std::string>>());
    if (t.empty()) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply at least one import target."));
    }

    r.targets.reserve(t.size());
    using std::filesystem::absolute;
    std::ranges::transform(t, std::back_inserter(r.targets),
        [](const auto& s) { return absolute(s); });
    return r;
}

/**
 * @brief Reads the ore_export configuration from the variables map.
 */
export_options read_export_options(const variables_map& vm) {
    export_options r;

    r.target_entity = read_entity(vm);
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
ores::cli::config::delete_options read_delete_options(const variables_map& vm) {
    ores::cli::config::delete_options r;

    r.target_entity = read_entity(vm);

    if (vm.count(delete_key_arg) == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --key argument for delete command."));
    }
    r.key = vm[delete_key_arg].as<std::string>();

    return r;
}

/**
 * @brief Reads the add configuration from the variables map for currencies.
 */
ores::cli::config::add_options read_add_currency_options(const variables_map& vm) {
    ores::cli::config::add_options r;

    r.target_entity = entity::currencies;

    if (vm.count("modified-by") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --modified-by for add command."));
    }
    r.modified_by = vm["modified-by"].as<std::string>();

    // Currency-specific required fields
    if (vm.count("iso-code") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --iso-code for add currency command."));
    }
    r.iso_code = vm["iso-code"].as<std::string>();

    if (vm.count("name") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --name for add currency command."));
    }
    r.name = vm["name"].as<std::string>();

    // Optional currency fields with defaults
    if (vm.count("numeric-code") != 0)
        r.numeric_code = vm["numeric-code"].as<std::string>();
    if (vm.count("symbol") != 0)
        r.symbol = vm["symbol"].as<std::string>();
    if (vm.count("fraction-symbol") != 0)
        r.fraction_symbol = vm["fraction-symbol"].as<std::string>();
    if (vm.count("fractions-per-unit") != 0)
        r.fractions_per_unit = vm["fractions-per-unit"].as<int>();
    if (vm.count("rounding-type") != 0)
        r.rounding_type = vm["rounding-type"].as<std::string>();
    if (vm.count("rounding-precision") != 0)
        r.rounding_precision = vm["rounding-precision"].as<int>();
    if (vm.count("format") != 0)
        r.format = vm["format"].as<std::string>();
    if (vm.count("currency-type") != 0)
        r.currency_type = vm["currency-type"].as<std::string>();

    return r;
}

}

std::optional<options>
handle_currencies_command(bool has_help,
    const parsed_options& po,
    std::ostream& info,
    variables_map& vm) {

    // Collect all unrecognized options from the first pass
    auto o(collect_unrecognized(po.options, include_positional));
    o.erase(o.begin()); // Remove command name

    // Show help for currencies command if requested with no operation
    if (has_help && o.empty()) {
        const std::vector<std::pair<std::string, std::string>> operations = {
            {"import", "Import currencies from ORE XML files"},
            {"export", "Export currencies to ORE XML or CSV (external formats)"},
            {"list", "List currencies as JSON or table (internal formats)"},
            {"delete", "Delete a currency by ISO code"},
            {"add", "Add currencies from JSON files"}
        };
        print_entity_help("currencies", "Manage currencies", operations, info);
        return {};
    }

    if (o.empty()) {
        BOOST_THROW_EXCEPTION(parser_exception(
            "currencies command requires an operation (import, export, list, delete, add)"));
    }

    const auto operation = o.front();
    o.erase(o.begin()); // Remove operation from args

    // Validate operation
    validate_operation("currencies", operation, allowed_operations);

    options r;
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("CLI"));

    if (operation == import_command_name) {
        auto d = add_common_options(make_import_options_description());
        if (has_help) {
            print_help_command("currencies import", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        force_entity(vm, entity::currencies);
        r.importing = read_import_options(vm);
    } else if (operation == export_command_name) {
        auto d = add_common_options(make_export_options_description());
        if (has_help) {
            print_help_command("currencies export", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        force_entity(vm, entity::currencies);
        r.exporting = read_export_options(vm);
    } else if (operation == delete_command_name) {
        auto d = add_common_options(make_delete_options_description());
        if (has_help) {
            print_help_command("currencies delete", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        force_entity(vm, entity::currencies);
        r.deleting = read_delete_options(vm);
    } else if (operation == list_command_name) {
        auto d = add_common_options(make_export_options_description());
        if (has_help) {
            print_help_command("currencies list", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        force_entity(vm, entity::currencies);
        r.exporting = read_export_options(vm);
    } else if (operation == add_command_name) {
        auto d = add_common_options(make_add_currency_options_description());
        if (has_help) {
            print_help_command("currencies add", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        force_entity(vm, entity::currencies);
        r.adding = read_add_currency_options(vm);
    }

    // Read common options
    using ores::utility::database::database_configuration;
    using ores::utility::log::logging_configuration;
    r.database = database_configuration::read_options(vm);
    r.logging = logging_configuration::read_options(vm);

    return r;
}

}
