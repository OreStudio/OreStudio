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

#include <iostream>
#include <boost/program_options.hpp>
#include "ores.utility/version/version.hpp"

namespace ores::comms::analyser::config {

namespace po = boost::program_options;

std::optional<options> parser::parse(int argc, const char* argv[]) {
    options opts;
    std::string cmd_str;

    po::options_description visible("Options");
    visible.add_options()
        ("help,h", "Display this help message")
        ("version,V", "Display version information")
        ("verbose,v", "Enable verbose output");

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
                  << "Usage: ores.comms.analyser [command] <session-file> [options]\n\n"
                  << "Commands:\n"
                  << "  read    Read and display session frames (default)\n"
                  << "  info    Display session metadata only\n\n"
                  << visible << "\n"
                  << "Examples:\n"
                  << "  ores.comms.analyser read session-20250115-143205-abc123.ores\n"
                  << "  ores.comms.analyser info session.ores\n"
                  << "  ores.comms.analyser session.ores -v\n";
        return std::nullopt;
    }

    if (vm.count("version")) {
        std::cout << "ores.comms.analyser version "
                  << ::ores::utility::version::version::current_version() << "\n";
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
        throw std::runtime_error("No input file specified. "
            "Use --help for usage information.");
    }

    opts.verbose = vm.count("verbose") > 0;

    return opts;
}

}
