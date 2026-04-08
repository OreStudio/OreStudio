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
#include <openssl/crypto.h>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/scope_exit.hpp>
#include <Wt/WServer.h>
#include <Wt/WLogSink.h>
#include "ores.wt.service/config/parser.hpp"
#include "ores.wt.service/config/parser_exception.hpp"
#include "ores.wt.service/service/application_context.hpp"
#include "ores.wt.service/app/ore_application.hpp"
#include "ores.wt.service/messaging/registrar.hpp"
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/wt_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"

namespace {

using namespace ores::logging;
using ores::platform::environment::environment;

const std::string default_address = "0.0.0.0";
const std::string default_port = "8080";

constexpr std::string_view service_name = "ores.wt.service";
constexpr std::string_view service_version = ORES_VERSION;

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
    explicit boost_log_sink()
        : lg_(ores::logging::make_logger("ores.wt.service.wt")) {}

    void log(const std::string& type, const std::string& /*scope*/,
             const std::string& message) const noexcept override {
        try {
            using namespace ores::logging;
            if (type == "debug")
                BOOST_LOG_SEV(lg_, debug) << message;
            else if (type == "warning")
                BOOST_LOG_SEV(lg_, warn) << message;
            else if (type == "error" || type == "fatal")
                BOOST_LOG_SEV(lg_, error) << message;
            else
                BOOST_LOG_SEV(lg_, info) << message;
        } catch (...) {}
    }

private:
    mutable ores::logging::logger_t lg_;
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

    // Initialise the application context (internal DB context + eventing).
    auto& app_ctx = ores::wt::service::application_context::instance();
    app_ctx.initialize(opts.database);
    app_ctx.start_eventing();

    // Build Wt argv.
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
        for (size_t i = 0; i < result.wt_args.size(); ++i) {
            if (result.wt_args[i] == "--http-address" && i + 1 < result.wt_args.size()) {
                http_address = result.wt_args[i + 1];
            } else if (result.wt_args[i] == "--http-port" && i + 1 < result.wt_args.size()) {
                http_port = result.wt_args[i + 1];
            }
        }
    }

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

    // Construct NATS client and runner context.
    ores::nats::service::client nats(opts.nats);
    nats.connect();
    BOOST_LOG_SEV(lg, info) << "Connected to NATS: " << opts.nats.url
                            << " (namespace: '"
                            << (opts.nats.subject_prefix.empty() ? "(none)" : opts.nats.subject_prefix)
                            << "')";

    ores::database::context_factory::configuration db_cfg {
        .database_options = opts.database,
        .pool_size = 2,
        .num_attempts = 10,
        .wait_time_in_seconds = 1,
        .service_account = opts.database.user
    };
    auto ctx = ores::database::context_factory::make_context(db_cfg);

    ores::service::service::run_wt(
        nats, std::move(ctx), service_name,
        [](auto& n, auto c, auto v) {
            return ores::wt::service::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v));
        },
        [&]() {
            boost_log_sink wt_sink;
            Wt::WServer server(argv[0]);
            server.setServerConfiguration(wt_argc, wt_argv.data());
            server.setCustomLogger(wt_sink);
            server.addEntryPoint(Wt::EntryPointType::Application, &create_application);

            if (server.start()) {
                BOOST_LOG_SEV(lg, info) << "Waiting for requests...";
                Wt::WServer::waitForShutdown();
                server.stop();
            }
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc,
                [hb]() { return hb->run(); },
                boost::asio::detached);
        });

    app_ctx.stop_eventing();
    BOOST_LOG_SEV(lg, info) << "Service stopped.";

    return EXIT_SUCCESS;
}

}

int main(int argc, char* argv[]) {
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
