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
#include <iostream>
#include <cstdlib>
#include <openssl/crypto.h>
#include <boost/scope_exit.hpp>
#include <Wt/WServer.h>
#include "ores.wt/config/parser.hpp"
#include "ores.wt/config/parser_exception.hpp"
#include "ores.wt/service/application_context.hpp"
#include "ores.wt/app/ore_application.hpp"
#include "ores.telemetry/log/lifecycle_manager.hpp"

namespace {

std::unique_ptr<Wt::WApplication>
create_application(const Wt::WEnvironment& env) {
    return std::make_unique<ores::wt::app::ore_application>(env);
}

int run(int argc, char* argv[]) {
    std::vector<std::string> args;
    for (int i = 1; i < argc; ++i) {
        args.emplace_back(argv[i]);
    }

    ores::wt::config::parser parser;
    auto result = parser.parse(args, std::cout, std::cerr);

    if (!result.opts.has_value()) {
        return EXIT_SUCCESS;
    }

    const auto& opts = result.opts.value();

    std::unique_ptr<ores::telemetry::log::lifecycle_manager> log_mgr;
    if (opts.logging.has_value()) {
        log_mgr = std::make_unique<ores::telemetry::log::lifecycle_manager>(
            opts.logging.value());
    }

    auto& app_ctx = ores::wt::service::application_context::instance();
    app_ctx.initialize(opts.database);

    std::vector<std::string> wt_argv_strings;
    wt_argv_strings.push_back(argv[0]);
    for (const auto& arg : result.wt_args) {
        wt_argv_strings.push_back(arg);
    }

    if (result.wt_args.empty()) {
        wt_argv_strings.push_back("--docroot");
        wt_argv_strings.push_back(".");
        wt_argv_strings.push_back("--http-address");
        wt_argv_strings.push_back("0.0.0.0");
        wt_argv_strings.push_back("--http-port");
        wt_argv_strings.push_back("8080");
    }

    std::vector<char*> wt_argv;
    for (auto& s : wt_argv_strings) {
        wt_argv.push_back(s.data());
    }
    wt_argv.push_back(nullptr);

    int wt_argc = static_cast<int>(wt_argv.size() - 1);

    return Wt::WRun(wt_argc, wt_argv.data(), &create_application);
}

}

int main(int argc, char* argv[]) {
    BOOST_SCOPE_EXIT(void) {
        OPENSSL_cleanup();
    } BOOST_SCOPE_EXIT_END

    try {
        return run(argc, argv);
    } catch (const ores::wt::config::parser_exception&) {
        return EXIT_FAILURE;
    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
}
