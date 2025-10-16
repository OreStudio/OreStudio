/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include <iostream>
#include <optional>
#include <memory>
#include <thread>
#include <boost/throw_exception.hpp>
#include <boost/cobalt.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/detached.hpp>
#include <sqlgen/postgres.hpp>
#include <magic_enum/magic_enum.hpp>
#include <cli/cli.h>
#include <cli/clifilesession.h>
#include "ores.cli/config/export_options.hpp"
#include "ores.utility/log/logger.hpp"
#include "ores.utility/streaming/std_vector.hpp"
#include "ores.risk/orexml/importer.hpp"
#include "ores.risk/orexml/exporter.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.risk/repository/context_factory.hpp"
#include "ores.cli/app/application_exception.hpp"
#include "ores.cli/app/application.hpp"
#include "ores.comms/client.hpp"
#include "ores.risk/messaging/protocol.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.cli.application"));

}

namespace ores::cli::app {

using risk::orexml::importer;
using risk::orexml::exporter;
using ores::risk::domain::currency;
using risk::repository::currency_repository;
using connection = sqlgen::Result<rfl::Ref<sqlgen::postgres::Connection>>;

risk::repository::context application::make_context() {
    using configuration = risk::repository::context_factory::configuration;
    configuration cfg {
        .user = "ores",
        .password = "ahV6aehuij6eingohsiajaiT0",
        .host = "localhost",
        .database = "oresdb",
        .port = 5434,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    using risk::repository::context_factory;
    return context_factory::make_context(cfg);
}

application::application() : context_(make_context()) {
    BOOST_LOG_SEV(lg, debug) << "Creating application.";
}

void application::
import_currencies(const std::vector<std::filesystem::path> files) const {
    for (const auto& f : files) {
        BOOST_LOG_SEV(lg, debug) << "Processing file: " << f;
        auto ccys(importer::import_currency_config(f));
        currency_repository rp;
        rp.write(context_, ccys);
        std::cout << ccys << std::endl;
    }
}

void application::
import_data(const std::optional<config::import_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "No importing configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currencies:
            import_currencies(cfg.targets);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Unsupported entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

void application::
export_currencies(const config::export_options& cfg) const {
    BOOST_LOG_SEV(lg, debug) << "Exporting currency configurations.";
    risk::repository::currency_repository rp;

    const auto reader([&]() {
        if (cfg.all_versions) {
            BOOST_LOG_SEV(lg, debug) << "Reading all versions for currencies.";
            if (cfg.key.empty())
                return rp.read_all(context_);
            else
                return rp.read_all(context_, cfg.key);
        } else if (cfg.as_of.empty()) {
            BOOST_LOG_SEV(lg, debug) << "Reading latest currencies.";
            if (cfg.key.empty())
                return rp.read_latest(context_);
            else
                return rp.read_latest(context_, cfg.key);
        }
        BOOST_LOG_SEV(lg, debug) << "Reading currencies as of: " << cfg.as_of;
        if (cfg.key.empty())
            return rp.read_at_timepoint(context_, cfg.as_of);
        else
            return rp.read_at_timepoint(context_, cfg.as_of, cfg.key);
    });

    const std::vector<currency> ccys(reader());
    if (cfg.target_format == config::format::xml) {
        std::string ccy_cfgs = exporter::export_currency_config(ccys);
        std::cout << ccy_cfgs << std::endl;
    } else if (cfg.target_format == config::format::json) {
        std::cout << ccys << std::endl;
    }
}

void application::
export_data(const std::optional<config::export_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "No dumping configuration found.";
        return;
    }

    const auto& cfg(ocfg.value());
    switch (cfg.target_entity) {
        case config::entity::currencies:
            export_currencies(cfg);
            break;
        default:
            BOOST_THROW_EXCEPTION(
                application_exception(std::format("Unsupported entity: {}",
                        magic_enum::enum_name(cfg.target_entity))));
    }
}

boost::cobalt::promise<void>
application::run_client() const {
    BOOST_LOG_SEV(lg, info) << "Starting client REPL.";

    try {
        // Get the current executor from cobalt context
        auto executor = co_await boost::cobalt::this_coro::executor;
        boost::asio::io_context ctx;
        std::thread executor_thread([&ctx]() { ctx.run(); });

        // Shared client state
        std::shared_ptr<comms::client> client;
        comms::client_options current_config{
            .host = "localhost",
            .port = 55555,
            .client_identifier = "ores-client",
            .verify_certificate = false
        };

        // Create CLI root menu
        auto rootMenu = std::make_unique<::cli::Menu>("ores-client");

        // Add CONNECT command
        rootMenu->Insert(
            "connect",
            [&client, &current_config, executor](std::ostream& out, std::string host, std::string port, std::string identifier) {
                try {
                    // Update config with provided arguments (if any)
                    if (!host.empty()) {
                        current_config.host = host;
                    }
                    if (!port.empty()) {
                        try {
                            current_config.port = static_cast<std::uint16_t>(std::stoi(port));
                        } catch (...) {
                            out << "✗ Invalid port number: " << port << std::endl;
                            return;
                        }
                    }
                    if (!identifier.empty()) {
                        current_config.client_identifier = identifier;
                    }

                    out << "Connecting to " << current_config.host << ":" << current_config.port
                        << " (identifier: " << current_config.client_identifier << ")..." << std::endl;

                    // Disconnect existing client if any
                    if (client && client->is_connected()) {
                        out << "Disconnecting existing connection..." << std::endl;
                        client->disconnect();
                    }

                    // Make a copy of config for the async operation
                    comms::client_options config_copy = current_config;

                    // Spawn cobalt task to connect
                    boost::cobalt::spawn(executor,
                        [config_copy, &client, executor]() -> boost::cobalt::task<void> {
                            try {
                                BOOST_LOG_SEV(lg, debug) << "Starting async connect...";
                                auto new_client = std::make_shared<comms::client>(config_copy, executor);
                                bool connected = co_await new_client->connect();
                                BOOST_LOG_SEV(lg, debug) << "Connect result: " << connected;
                                if (connected) {
                                    client = new_client;
                                    std::cout << "✓ Successfully connected!" << std::endl;
                                } else {
                                    std::cout << "✗ Connection failed" << std::endl;
                                }
                            } catch (const std::exception& e) {
                                BOOST_LOG_SEV(lg, error) << "Connect exception: " << e.what();
                                std::cout << "✗ Connection error: " << e.what() << std::endl;
                            }
                        }(),
                        boost::asio::detached);

                } catch (const std::exception& e) {
                    out << "✗ Error during connection: " << e.what() << std::endl;
                }
            },
            "Connect to server (optional: host port identifier)");

        // Add DISCONNECT command
        rootMenu->Insert(
            "disconnect",
            [&client](std::ostream& out) {
                if (!client) {
                    out << "Not connected to any server" << std::endl;
                    return;
                }

                if (!client->is_connected()) {
                    out << "Already disconnected" << std::endl;
                    client.reset();
                    return;
                }

                client->disconnect();
                client.reset();
                out << "✓ Disconnected from server" << std::endl;
            },
            "Disconnect from server");

        // Add CURRENCIES submenu
        auto currenciesMenu = std::make_unique<::cli::Menu>("currencies");

        // Add CURRENCIES GET command
        currenciesMenu->Insert(
            "get",
            [&client, executor](std::ostream& out) {
                if (!client || !client->is_connected()) {
                    out << "✗ Not connected to server. Use 'connect' command first." << std::endl;
                    return;
                }

                out << "Retrieving currencies from server..." << std::endl;

                try {
                    // Spawn cobalt task to get currencies
                    boost::cobalt::spawn(executor,
                        [client]() -> boost::cobalt::task<void> {
                            try {
                                // Create get_currencies request
                                ores::risk::messaging::get_currencies_request request;
                                auto request_payload = request.serialize();

                                // Create request frame
                                comms::protocol::frame request_frame(
                                    comms::protocol::message_type::get_currencies_request,
                                    0, // sequence will be set by send_request
                                    std::move(request_payload));

                                // Send request and get response asynchronously
                                auto response_result = co_await client->send_request(std::move(request_frame));

                                if (!response_result) {
                                    std::cout << "✗ Request failed with error code: "
                                              << static_cast<int>(response_result.error()) << std::endl;
                                    co_return;
                                }

                                // Deserialize response
                                auto response = ores::risk::messaging::get_currencies_response::deserialize(
                                    response_result->payload());
                                if (!response) {
                                    std::cout << "✗ Failed to deserialize response, error code: "
                                              << static_cast<int>(response.error()) << std::endl;
                                    co_return;
                                }

                                // Display results
                                std::cout << "✓ Successfully retrieved " << response->currencies.size() << " currencies" << std::endl;
                                std::cout << std::endl;
                                std::cout << "Currencies (JSON):" << std::endl;
                                std::cout << response->currencies << std::endl;

                            } catch (const std::exception& e) {
                                std::cout << "✗ Error: " << e.what() << std::endl;
                            }
                        }(),
                        boost::asio::detached);

                } catch (const std::exception& e) {
                    out << "✗ Error: " << e.what() << std::endl;
                }
            },
            "Retrieve all currencies from the server");

        // Add currencies submenu to root
        rootMenu->Insert(std::move(currenciesMenu));

        // Create CLI and session
        ::cli::Cli cli_instance(std::move(rootMenu));
        cli_instance.ExitAction([](auto& out) { out << "Goodbye!" << std::endl; });

        // Use stdin/stdout for the session
        ::cli::CliFileSession session(cli_instance, std::cin, std::cout);

        // Print welcome message
        std::cout << "ORE Studio Client REPL" << std::endl;
        std::cout << "Type 'help' for available commands, 'exit' to quit" << std::endl;
        std::cout << std::endl;

        // Run the REPL session
        session.Start();

        BOOST_LOG_SEV(lg, info) << "Client REPL session ended.";

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Client error: " << e.what();
    }

    co_return;
}

boost::cobalt::promise<void> application::run(const config::options& cfg) const {
    BOOST_LOG_SEV(lg, info) << "Started application.";

    import_data(cfg.importing);
    export_data(cfg.exporting);

    // If any client operation was requested, run REPL
    if (cfg.client) {
        co_await run_client();
    }

    BOOST_LOG_SEV(lg, info) << "Finished application.";
}

}
