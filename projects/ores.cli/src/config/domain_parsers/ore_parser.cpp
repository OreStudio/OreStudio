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
#include "ores.cli/config/domain_parsers/ore_parser.hpp"
#include "ores.cli/config/ore_roundtrip_options.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/parser_helpers.hpp"
#include "ores.logging/logging_configuration.hpp"
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <filesystem>
#include <format>
#include <iomanip>

namespace ores::cli::config::domain_parsers {

namespace {

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::positional_options_description;
using boost::program_options::command_line_parser;
using boost::program_options::include_positional;
using boost::program_options::collect_unrecognized;

using ores::cli::config::options;
using ores::cli::config::parser_exception;
using ores::cli::config::ore_roundtrip_options;
using ores::cli::config::parser_helpers::print_help_command;

const std::string roundtrip_name("roundtrip");
const std::string
    roundtrip_desc("Roundtrip all portfolio XML files through the import→export pipeline. "
                   "No database access required.");

void print_domain_help(std::ostream& info) {
    info << "ore: ORE XML import/export utilities." << std::endl << std::endl;

    const unsigned int command_width(15);
    auto row([&](const std::string& name, const std::string& desc) {
        info << "  " << std::setfill(' ') << std::left << std::setw(command_width) << name << desc
             << std::endl;
    });

    row(roundtrip_name, roundtrip_desc);
    info << std::endl << "Use: ores.cli ore <command> --help for details." << std::endl;
}

options_description make_roundtrip_options_description() {
    options_description r("Roundtrip options");
    r.add_options()("input-dir",
                    value<std::string>(),
                    "Directory containing ORE portfolio XML files (recursive).")(
        "output-dir",
        value<std::string>(),
        "Directory to write roundtrip outputs (mirrors input tree).");
    return r;
}

ore_roundtrip_options read_roundtrip_options(const variables_map& vm) {
    if (vm.count("input-dir") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply <input-dir> for ore roundtrip command."));
    }
    if (vm.count("output-dir") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --output-dir for ore roundtrip command."));
    }

    using std::filesystem::absolute;
    ore_roundtrip_options r;
    r.input_dir = absolute(vm["input-dir"].as<std::string>());
    r.output_dir = absolute(vm["output-dir"].as<std::string>());
    return r;
}

}

std::optional<options>
handle_ore_command(bool has_help, const parsed_options& po, std::ostream& info, variables_map& vm) {

    auto unrecognized = collect_unrecognized(po.options, include_positional);
    // unrecognized[0] = "ore", unrecognized[1] = command, rest = command args
    std::vector<std::string> command_args;
    if (unrecognized.size() > 2)
        command_args = std::vector<std::string>(unrecognized.begin() + 2, unrecognized.end());

    const std::string command_name =
        vm.count("command") ? vm["command"].as<std::string>() : std::string{};

    if (has_help && command_name.empty()) {
        print_domain_help(info);
        return {};
    }

    if (command_name.empty()) {
        BOOST_THROW_EXCEPTION(parser_exception("ore command requires a subcommand (roundtrip)."));
    }

    if (command_name != roundtrip_name) {
        BOOST_THROW_EXCEPTION(parser_exception(std::format(
            "Invalid or unsupported ore command: {}. Available: roundtrip", command_name)));
    }

    // Parse roundtrip-specific options
    positional_options_description pos;
    pos.add("input-dir", 1);

    auto d = make_roundtrip_options_description();
    if (has_help) {
        print_help_command("ore roundtrip <input-dir> --output-dir <dir>", d, info);
        return {};
    }

    auto new_po = command_line_parser(command_args).options(d).positional(pos).run();

    variables_map new_vm;
    store(new_po, new_vm);
    notify(new_vm);

    options r;
    r.ore_roundtrip = read_roundtrip_options(new_vm);
    r.logging = ores::logging::logging_configuration::read_options(vm);
    return r;
}

}
