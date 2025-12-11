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
#include "ores.cli/config/parser_helpers.hpp"

#include <format>
#include <iomanip>
#include <ostream>
#include <algorithm>
#include <boost/throw_exception.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.database/database_configuration.hpp"
#include "ores.cli/config/parser_exception.hpp"

namespace ores::cli::config::parser_helpers {

namespace {
const std::string indent("   ");
const std::string export_as_of_arg("as-of");
const std::string export_key_arg("key");
const std::string export_all_versions_arg("all-versions");
const std::string export_format_arg("format");
const std::string delete_key_arg("key");
}

void print_help_header(std::ostream& s) {
    s << "ORE Studio is a User Interface for Open Source Risk Engine (ORE)."
      << std::endl
      << "CLI provides a command line version of the interface." << std::endl
      << "ORE Studio is created by the ORE Studio project. " << std::endl;
}

void print_help_command(const std::string& command_name,
    const boost::program_options::options_description& od, std::ostream& info) {
    print_help_header(info);
    info << "Displaying options specific to the '" << command_name << "' command. "
         << std::endl
         << "For global options, type --help." << std::endl << std::endl
         << od;
}

boost::program_options::options_description
add_common_options(boost::program_options::options_description base) {
    using ores::database::database_configuration;
    using ores::utility::log::logging_configuration;

    const auto db_desc(database_configuration::make_options_description());
    const auto log_desc(logging_configuration::make_options_description("ores.cli.log"));

    boost::program_options::options_description r;
    r.add(base).add(db_desc).add(log_desc);
    return r;
}

void validate_operation(const std::string& entity_name,
    const std::string& operation,
    const std::vector<std::string>& allowed_operations) {

    if (std::ranges::find(allowed_operations, operation) == allowed_operations.end()) {
        std::string ops_list;
        for (size_t i = 0; i < allowed_operations.size(); ++i) {
            if (i > 0) ops_list += ", ";
            ops_list += allowed_operations[i];
        }

        throw parser_exception(std::format(
            "Invalid operation for {}: {}. Valid operations: {}",
            entity_name, operation, ops_list));
    }
}

void print_entity_help(const std::string& entity_name,
    const std::string& description,
    const std::vector<std::pair<std::string, std::string>>& operations,
    std::ostream& info) {

    info << entity_name << " - " << description << std::endl << std::endl;
    info << "Usage: ores.cli " << entity_name << " <operation> [options]"
         << std::endl << std::endl;
    info << "Available operations:" << std::endl;

    const unsigned int operation_width(15);
    for (const auto& [op_name, op_desc] : operations) {
        info << indent << std::setfill(' ') << std::left
             << std::setw(operation_width) << op_name << op_desc << std::endl;
    }

    info << std::endl << "For operation-specific options, use: "
         << entity_name << " <operation> --help" << std::endl;
}

boost::program_options::options_description make_export_options_description() {
    using boost::program_options::options_description;
    using boost::program_options::value;

    options_description r("Export");
    r.add_options()
        ("as-of", value<std::string>(),
            "Time point from which to dump data. If not supplied, defaults to latest.")
        ("key", value<std::string>(), "Key to filter data by.")
        ("all-versions", "If supplied, retrieves all versions.")
        ("format", value<std::string>(), "Format to export data in, e.g. xml or json.");

    return r;
}

boost::program_options::options_description make_delete_options_description() {
    using boost::program_options::options_description;
    using boost::program_options::value;

    options_description r("Delete");
    r.add_options()
        ("key", value<std::string>(), "Key to identify the entity (e.g., account ID or username).");

    return r;
}

format read_format(const boost::program_options::variables_map& vm) {
    if (vm.count(export_format_arg) == 0)
        return format::json;

    const auto s(vm[export_format_arg].as<std::string>());
    auto f = magic_enum::enum_cast<format>(s);
    if (f.has_value())
        return f.value();

    BOOST_THROW_EXCEPTION(
        parser_exception("Invalid or unsupported format: '" + s + "'"));
}

export_options read_export_options(
    const boost::program_options::variables_map& vm, entity e) {
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

delete_options read_delete_options(
    const boost::program_options::variables_map& vm, entity e) {
    delete_options r;

    r.target_entity = e;

    if (vm.count(delete_key_arg) == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --key argument for delete command."));
    }
    r.key = vm[delete_key_arg].as<std::string>();

    return r;
}

}
