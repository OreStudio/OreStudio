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
#include "ores.cli/config/entity_parsers/compute_hosts_parser.hpp"

#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.cli/config/parser_helpers.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/add_compute_host_options.hpp"
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
using ores::cli::config::parser_helpers::read_export_options;

const std::string list_command_name("list");
const std::string add_command_name("add");

const std::vector<std::string> allowed_operations{
    list_command_name, add_command_name
};

options_description make_add_compute_host_options_description() {
    options_description r("Add Compute Host Options");
    r.add_options()
        ("external-id",
            value<std::string>(),
            "UUID identifying this node's host record (required)")
        ("location",
            value<std::string>()->default_value(""),
            "Deployment location, e.g. 'us-east-1' or 'office-rack-1'")
        ("cpu-count",
            value<int>()->default_value(0),
            "Number of logical CPUs available on this host")
        ("ram-mb",
            value<int>()->default_value(0),
            "Total RAM in MB available on this host")
        ("gpu-type",
            value<std::string>()->default_value(""),
            "GPU model string, e.g. 'NVIDIA RTX 4090' (leave blank if none)")
        ("modified-by",
            value<std::string>(),
            "Username of the admin registering this host (required)");

    return r;
}

ores::cli::config::add_compute_host_options
read_add_compute_host_options(const variables_map& vm) {
    ores::cli::config::add_compute_host_options r;

    if (vm.count("modified-by") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --modified-by for add command."));
    }
    r.modified_by = vm["modified-by"].as<std::string>();

    if (vm.count("external-id") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --external-id for add host command."));
    }
    r.external_id = vm["external-id"].as<std::string>();

    if (vm.count("location") != 0) {
        const auto v = vm["location"].as<std::string>();
        if (!v.empty()) r.location = v;
    }
    if (vm.count("cpu-count") != 0) {
        const auto v = vm["cpu-count"].as<int>();
        if (v > 0) r.cpu_count = v;
    }
    if (vm.count("ram-mb") != 0) {
        const auto v = vm["ram-mb"].as<int>();
        if (v > 0) r.ram_mb = v;
    }
    if (vm.count("gpu-type") != 0) {
        const auto v = vm["gpu-type"].as<std::string>();
        if (!v.empty()) r.gpu_type = v;
    }

    return r;
}

}

std::optional<options>
handle_compute_hosts_command(bool has_help,
    const parsed_options& po,
    std::ostream& info,
    variables_map& vm) {

    auto o(collect_unrecognized(po.options, include_positional));
    o.erase(o.begin());

    if (has_help && o.empty()) {
        const std::vector<std::pair<std::string, std::string>> operations = {
            {"list", "List compute hosts as JSON or table"},
            {"add",  "Register a new compute host node"}
        };
        print_entity_help("hosts", "Manage compute hosts", operations, info);
        return {};
    }

    if (o.empty()) {
        BOOST_THROW_EXCEPTION(parser_exception(
            "hosts command requires an operation (list, add)"));
    }

    const auto operation = o.front();
    o.erase(o.begin());

    validate_operation("hosts", operation, allowed_operations);

    options r;
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("CLI"));

    if (operation == list_command_name) {
        auto d = add_common_options(make_export_options_description());
        if (has_help) {
            print_help_command("hosts list", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.exporting = read_export_options(vm, entity::compute_hosts);
    } else if (operation == add_command_name) {
        auto d = add_common_options(make_add_compute_host_options_description());
        if (has_help) {
            print_help_command("hosts add", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.adding = read_add_compute_host_options(vm);
    }

    using ores::database::database_configuration;
    using ores::logging::logging_configuration;
    r.database = database_configuration::read_options(vm);
    r.logging = logging_configuration::read_options(vm);

    return r;
}

}
