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
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.utility/version/version.hpp"

namespace {

using namespace ores::telemetry::log;

const std::string default_address = "0.0.0.0";
const std::string default_port = "8080";

std::unique_ptr<Wt::WApplication>
create_application(const Wt::WEnvironment& env) {
    return std::make_unique<ores::wt::app::ore_application>(env);
}

std::optional<std::string> get_env(const char* name) {
    const char* value = std::getenv(name);
    if (value != nullptr && value[0] != '\0') {
        return std::string(value);
    }
    return std::nullopt;
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

    auto lg(make_logger("ores.wt"));
    BOOST_LOG_SEV(lg, info) << "Starting ORE Studio Web v" << ORES_VERSION;

    auto& app_ctx = ores::wt::service::application_context::instance();
    app_ctx.initialize(opts.database);

    std::vector<std::string> wt_argv_strings;
    wt_argv_strings.push_back(argv[0]);
    for (const auto& arg : result.wt_args) {
        wt_argv_strings.push_back(arg);
    }

    std::string http_address = default_address;
    std::string http_port = default_port;

    if (result.wt_args.empty()) {
        wt_argv_strings.push_back("--docroot");
        wt_argv_strings.push_back(".");
        wt_argv_strings.push_back("--http-address");
        wt_argv_strings.push_back(default_address);
        wt_argv_strings.push_back("--http-port");
        wt_argv_strings.push_back(default_port);
    } else {
        // Extract address and port from user-provided args for logging
        for (size_t i = 0; i < result.wt_args.size(); ++i) {
            if (result.wt_args[i] == "--http-address" && i + 1 < result.wt_args.size()) {
                http_address = result.wt_args[i + 1];
            } else if (result.wt_args[i] == "--http-port" && i + 1 < result.wt_args.size()) {
                http_port = result.wt_args[i + 1];
            }
        }
    }

    // Add resources directory if set via environment variable.
    // Note: Uses WT_RESOURCES_DIR (not ORES_WT_) to avoid conflict with
    // the ORES_WT_ prefix used by the option parser's environment mapper.
    auto resources_dir = get_env("WT_RESOURCES_DIR");
    if (resources_dir.has_value()) {
        wt_argv_strings.push_back("--resources-dir");
        wt_argv_strings.push_back(resources_dir.value());
        BOOST_LOG_SEV(lg, info) << "Using Wt resources from: " << resources_dir.value();
    }

    BOOST_LOG_SEV(lg, info) << "HTTP server listening on http://" << http_address
                           << ":" << http_port;

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
