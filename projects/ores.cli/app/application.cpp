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
#include <boost/throw_exception.hpp>
#include <boost/cobalt.hpp>
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
application::run_client(const std::optional<comms::client_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "No client configuration found.";
        co_return;
    }

    const auto& cfg(ocfg.value());
    BOOST_LOG_SEV(lg, info) << "Starting client REPL for "
                             << cfg.host << ":" << cfg.port;

    try {
        // Store client configuration for use in REPL commands
        const comms::client_options& client_cfg = cfg;

        // Create CLI root menu
        auto rootMenu = std::make_unique<::cli::Menu>("ores-client");

        // Add HANDSHAKE command
        rootMenu->Insert(
            "HANDSHAKE",
            [client_cfg](std::ostream& out) {
                out << "Performing handshake..." << std::endl;

                try {
                    // Track success
                    bool handshake_succeeded = false;
                    std::string error_message;

                    // Create a task that creates its own client and performs handshake
                    auto handshake_task = [&]() -> boost::cobalt::task<void> {
                        try {
                            // Create client with this coroutine's executor
                            auto cli = std::make_shared<comms::client>(
                                client_cfg,
                                co_await boost::cobalt::this_coro::executor);

                            bool connected = co_await cli->connect();
                            if (!connected) {
                                error_message = "Failed to connect to server";
                                co_return;
                            }

                            handshake_succeeded = true;

                            // Disconnect after successful handshake
                            cli->disconnect();
                        } catch (const std::exception& e) {
                            error_message = std::string("Exception: ") + e.what();
                        }
                    };

                    // Run the task synchronously
                    boost::cobalt::run(handshake_task());

                    // Report results
                    if (handshake_succeeded) {
                        out << "✓ Successfully connected and performed handshake!" << std::endl;
                        out << "✓ Disconnected from server" << std::endl;
                    } else {
                        out << "✗ Handshake failed: " << error_message << std::endl;
                    }

                } catch (const std::exception& e) {
                    out << "✗ Error during handshake: " << e.what() << std::endl;
                }
            },
            "Perform handshake with the server");

        // Add CURRENCIES submenu
        auto currenciesMenu = std::make_unique<::cli::Menu>("currencies");

        // Add CURRENCIES GET command
        currenciesMenu->Insert(
            "get",
            [client_cfg](std::ostream& out) {
                out << "Retrieving currencies from server..." << std::endl;

                try {
                    bool success = false;
                    std::string error_message;
                    std::vector<ores::risk::domain::currency> currencies;

                    auto get_currencies_task = [&]() -> boost::cobalt::task<void> {
                        try {
                            // Create client
                            auto cli = std::make_shared<comms::client>(
                                client_cfg,
                                co_await boost::cobalt::this_coro::executor);

                            // Connect and handshake
                            bool connected = co_await cli->connect();
                            if (!connected) {
                                error_message = "Failed to connect to server";
                                co_return;
                            }

                            // Create get_currencies request
                            ores::risk::messaging::get_currencies_request request;
                            auto request_payload = request.serialize();

                            // Create request frame
                            comms::protocol::frame request_frame(
                                comms::protocol::message_type::get_currencies_request,
                                0, // sequence will be set by send_request
                                std::move(request_payload));

                            // Send request and get response
                            auto response_result = co_await cli->send_request(std::move(request_frame));
                            if (!response_result) {
                                error_message = std::format("Request failed with error code: {}",
                                    static_cast<int>(response_result.error()));
                                cli->disconnect();
                                co_return;
                            }

                            // Deserialize response
                            auto response = ores::risk::messaging::get_currencies_response::deserialize(
                                response_result->payload());
                            if (!response) {
                                error_message = std::format("Failed to deserialize response, error code: {}",
                                    static_cast<int>(response.error()));
                                cli->disconnect();
                                co_return;
                            }

                            currencies = std::move(response->currencies);
                            success = true;

                            // Disconnect
                            cli->disconnect();

                        } catch (const std::exception& e) {
                            error_message = std::string("Exception: ") + e.what();
                        }
                    };

                    // Run the task synchronously
                    boost::cobalt::run(get_currencies_task());

                    // Report results
                    if (success) {
                        out << "✓ Successfully retrieved " << currencies.size() << " currencies" << std::endl;
                        out << std::endl;
                        out << "Currencies (JSON):" << std::endl;
                        out << currencies << std::endl;
                    } else {
                        out << "✗ Failed to retrieve currencies: " << error_message << std::endl;
                    }

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

        BOOST_LOG_SEV(lg, info) << "Client REPL session ended";

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Client error: " << e.what();
    }
}

boost::cobalt::promise<void> application::run(const config::options& cfg) const {
    BOOST_LOG_SEV(lg, info) << "Started application.";

    import_data(cfg.importing);
    export_data(cfg.exporting);
    co_await run_client(cfg.client);

    BOOST_LOG_SEV(lg, info) << "Finished application.";
}

}
