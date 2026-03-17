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
#include <exception>
#include <openssl/crypto.h>
#include <boost/scope_exit.hpp>
#include <Wt/WServer.h>
#include <Wt/WLogSink.h>
#include "ores.wt.service/config/parser.hpp"
#include "ores.wt.service/config/parser_exception.hpp"
#include "ores.wt.service/service/application_context.hpp"
#include "ores.wt.service/app/ore_application.hpp"
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.platform/environment/environment.hpp"

namespace {

using namespace ores::logging;
using ores::platform::environment::environment;

const std::string default_address = "0.0.0.0";
const std::string default_port = "8080";

std::unique_ptr<Wt::WApplication>
create_application(const Wt::WEnvironment& env) {
    return std::make_unique<ores::wt::service::app::ore_application>(env);
}

/**
 * @brief Forwards Wt's internal log messages to Boost.Log.
 *
 * Installed via WServer::setCustomLogger() so that Wt library messages
 * (e.g. WServer/wthttp startup notices) are captured in the service log
 * file alongside application-level Boost.Log output.
 */
class boost_log_sink final : public Wt::WLogSink {
public:
    boost_log_sink() {
        std::cerr << "[DEBUG] boost_log_sink constructed\n";
    }
    ~boost_log_sink() {
        std::cerr << "[DEBUG] boost_log_sink destroyed\n";
    }
    void log(const std::string& type, const std::string& scope,
             const std::string& message) const noexcept override {
        std::cerr << "[DEBUG] boost_log_sink::log called type=" << type
                  << " scope=" << scope << "\n";
        try {
            using namespace ores::logging;
            static auto& lg = []() -> auto& {
                std::cerr << "[DEBUG] initialising static logger\n";
                static auto instance = make_logger("ores.wt.service.wt");
                return instance;
            }();
            std::cerr << "[DEBUG] about to BOOST_LOG_SEV\n";
            if (type == "debug")
                BOOST_LOG_SEV(lg, debug) << message;
            else if (type == "warning")
                BOOST_LOG_SEV(lg, warn) << message;
            else if (type == "error" || type == "fatal")
                BOOST_LOG_SEV(lg, error) << message;
            else
                BOOST_LOG_SEV(lg, info) << message;
            std::cerr << "[DEBUG] BOOST_LOG_SEV done\n";
        } catch (const std::exception& e) {
            std::cerr << "[DEBUG] boost_log_sink::log caught: " << e.what() << "\n";
        } catch (...) {
            std::cerr << "[DEBUG] boost_log_sink::log caught unknown exception\n";
        }
    }
};

int run(int argc, char* argv[]) {
    std::vector<std::string> args;
    for (int i = 1; i < argc; ++i) {
        args.emplace_back(argv[i]);
    }

    ores::wt::service::config::parser parser;
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

    auto lg(make_logger("ores.wt.service"));
    BOOST_LOG_SEV(lg, info) << ores::utility::version::format_startup_message(
        "ORE Studio Web");

    auto& app_ctx = ores::wt::service::application_context::instance();
    app_ctx.initialize(opts.database);
    app_ctx.start_eventing();

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
    auto resources_dir = environment::get_value("WT_RESOURCES_DIR");
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

    boost_log_sink wt_sink;
    Wt::WServer server(argv[0]);
    server.setServerConfiguration(wt_argc, wt_argv.data());
    server.setCustomLogger(wt_sink);
    server.addEntryPoint(Wt::EntryPointType::Application, &create_application);

    if (server.start()) {
        std::cerr << "[DEBUG] server started, waiting for shutdown\n";
        Wt::WServer::waitForShutdown();
        std::cerr << "[DEBUG] waitForShutdown returned, calling server.stop()\n";
        server.stop();
        std::cerr << "[DEBUG] server.stop() returned\n";
    }

    std::cerr << "[DEBUG] stopping eventing\n";
    app_ctx.stop_eventing();
    std::cerr << "[DEBUG] eventing stopped, logging final message\n";
    BOOST_LOG_SEV(lg, info) << "ORE Studio Web stopped";
    std::cerr << "[DEBUG] run() returning\n";

    return EXIT_SUCCESS;
}

}

int main(int argc, char* argv[]) {
    std::set_terminate([]() {
        std::cerr << "[DEBUG] std::terminate called\n";
        auto ep = std::current_exception();
        if (ep) {
            try { std::rethrow_exception(ep); }
            catch (const std::exception& e) {
                std::cerr << "[DEBUG] current exception: " << e.what() << "\n";
            } catch (...) {
                std::cerr << "[DEBUG] current exception: unknown\n";
            }
        }
        std::cerr << "[DEBUG] aborting\n";
        std::abort();
    });

    BOOST_SCOPE_EXIT(void) {
        OPENSSL_cleanup();
    } BOOST_SCOPE_EXIT_END

    try {
        return run(argc, argv);
    } catch (const ores::wt::service::config::parser_exception&) {
        return EXIT_FAILURE;
    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
}
