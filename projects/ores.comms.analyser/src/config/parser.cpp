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
#include "ores.comms.analyser/config/parser.hpp"

#include <filesystem>
#include <iostream>
#include <boost/program_options.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.utility/program_options/common_configuration.hpp"

namespace ores::comms::analyser::config {

namespace po = boost::program_options;

std::optional<options> parser::parse(int argc, const char* argv[]) {
    using ores::utility::program_options::common_configuration;

    options opts;
    std::string cmd_str;

    const auto visible = common_configuration::make_options_description();

    po::options_description hidden("Hidden options");
    hidden.add_options()
        ("command", po::value<std::string>(&cmd_str)->default_value("read"),
            "Command to execute (read, info)")
        ("input-file", po::value<std::string>(),
            "Input session file (.ores)");

    po::options_description all("All options");
    all.add(visible).add(hidden);

    po::positional_options_description positional;
    positional.add("command", 1);
    positional.add("input-file", 1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
        .options(all)
        .positional(positional)
        .run(), vm);

    if (vm.count("help")) {
        std::cout << "ores.comms.analyser - ORES session recording analyser\n\n"
                  << "Usage: ores.comms.analyser [command] <file-or-directory> [options]\n\n"
                  << "Commands:\n"
                  << "  read    Read and display session frames (default)\n"
                  << "  info    Display session metadata only\n\n"
                  << "Arguments:\n"
                  << "  file-or-directory   A .ores file or directory containing .ores files\n\n"
                  << visible << "\n"
                  << "Examples:\n"
                  << "  ores.comms.analyser read session.ores\n"
                  << "  ores.comms.analyser info session.ores\n"
                  << "  ores.comms.analyser info ./recordings/     # process all .ores in directory\n"
                  << "  ores.comms.analyser session.ores --verbose\n";
        return std::nullopt;
    }

    if (vm.count("version")) {
        std::cout << "Session Analyser for ORE Studio v" << ORES_VERSION << "\n"
                  << "Copyright (C) 2025 Marco Craveiro.\n"
                  << "License GPLv3: GNU GPL version 3 or later "
                  << "<http://gnu.org/licenses/gpl.html>.\n"
                  << "This is free software: you are free to change and redistribute it.\n"
                  << "There is NO WARRANTY, to the extent permitted by law.\n";

        const std::string build_info(ORES_BUILD_INFO);
        if (!build_info.empty()) {
            std::cout << build_info << "\n"
                      << "IMPORTANT: build details are NOT for security purposes.\n";
        }
        return std::nullopt;
    }

    po::notify(vm);

    // Parse command
    if (cmd_str == "read") {
        opts.cmd = command::read;
    } else if (cmd_str == "info") {
        opts.cmd = command::info;
    } else {
        // Treat as filename if not a recognized command
        opts.cmd = command::read;
        opts.input_file = cmd_str;
    }

    // Get input file
    if (vm.count("input-file")) {
        opts.input_file = vm["input-file"].as<std::string>();
    }

    if (opts.input_file.empty()) {
        throw std::runtime_error("No input file or directory specified. "
            "Use --help for usage information.");
    }

    // Validate path exists
    if (!std::filesystem::exists(opts.input_file)) {
        throw std::runtime_error("Path does not exist: " + opts.input_file.string());
    }

    opts.verbose = common_configuration::read_options(vm).verbose;

    return opts;
}

}
