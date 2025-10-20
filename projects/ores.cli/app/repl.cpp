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
#include <mutex>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <cli/cli.h>
#include <cli/clifilesession.h>
#include "ores.cli/app/repl.hpp"
#include "ores.utility/log/logger.hpp"
#include "ores.utility/streaming/std_vector.hpp"
#include "ores.risk/messaging/protocol.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.cli.repl"));
std::mutex cout_mutex;

}

namespace ores::cli::app {

repl::repl()
    : config_{
        .host = "localhost",
        .port = 55555,
        .client_identifier = "ores-client",
        .verify_certificate = false
    },
    client_(std::make_shared<comms::client>(config_)),
    io_ctx_(std::make_unique<boost::asio::io_context>()) {

    using boost::asio::executor_work_guard;
    using boost::asio::any_io_executor;
    work_guard_ = std::make_unique<executor_work_guard<any_io_executor>>(
        io_ctx_->get_executor());

    BOOST_LOG_SEV(lg, info) << "REPL created with default configuration.";
}

repl::~repl() {
    stop_io_thread();
}

void repl::run() {
    start_io_thread();
    display_welcome();

    auto cli_instance = setup_menus();
    ::cli::CliFileSession session(*cli_instance, std::cin, std::cout);
    session.Start();

    stop_io_thread();
    BOOST_LOG_SEV(lg, info) << "REPL session ended";
}

std::unique_ptr<::cli::Cli> repl::setup_menus() {
    auto root_menu =
        std::make_unique<::cli::Menu>("ores-client");

    register_connection_commands(*root_menu);
    register_currency_commands(*root_menu);

    auto cli_instance = std::make_unique<::cli::Cli>(std::move(root_menu));
    cli_instance->ExitAction([](auto& out) {
        out << "Goodbye!" << std::endl; });

    return cli_instance;
}

void repl::register_connection_commands(::cli::Menu& root_menu) {
    root_menu.Insert("connect",
        [this](std::ostream& out, std::string host, std::string port, std::string identifier) {
            try {
                auto executor = io_ctx_->get_executor();
                boost::asio::co_spawn(executor,
                    process_connect(std::move(host), std::move(port),
                        std::move(identifier)),
                    boost::asio::detached);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg, error) << "Error connecting: " << e.what();
                out << "✗ Error: " << e.what() << std::endl;
            }
        },
        "Connect to server (optional: host port identifier).");

    root_menu.Insert("disconnect",
        [this](std::ostream& out) {
            process_disconnect();
            out << "✓ Disconnected from server" << std::endl;
        },
        "Disconnect from server.");
}

void repl::register_currency_commands(::cli::Menu& root_menu) {
    auto currencies_menu = std::make_unique<::cli::Menu>("currencies");

    currencies_menu->Insert("get",
        [this](std::ostream& out) {
            if (!client_ || !client_->is_connected()) {
                out << "✗ Not connected to server. Use 'connect' command first." << std::endl;
                return;
            }

            BOOST_LOG_SEV(lg, debug) << "Retrieving currencies from server.";

            auto executor = io_ctx_->get_executor();
            boost::asio::co_spawn(executor,
                process_get_currencies(),
                boost::asio::detached);
        },
        "Retrieve all currencies from the server.");

    root_menu.Insert(std::move(currencies_menu));
}

boost::asio::awaitable<void> repl::
process_connect(std::string host, std::string port, std::string identifier) {

    try {
        if (!host.empty())
            config_.host = std::move(host);

        if (!port.empty()) {
            try {
                config_.port = static_cast<std::uint16_t>(std::stoi(port));
            } catch (...) {
                std::lock_guard<std::mutex> lock{cout_mutex};
                std::cout << "✗ Invalid port number: " << port << "\nores-client> " << std::flush;
                co_return;
            }
        }
        if (!identifier.empty())
            config_.client_identifier = std::move(identifier);

        {
            std::lock_guard<std::mutex> lock{cout_mutex};
            std::cout << "Connecting to " << config_.host << ":" << config_.port
                      << " (identifier: " << config_.client_identifier << ")..." << std::endl;
        }

        if (client_ && client_->is_connected()) {
            BOOST_LOG_SEV(lg, info) << "Disconnecting existing connection";
            client_->disconnect();
        }

        client_ = std::make_shared<comms::client>(config_);

        bool connected = co_await client_->connect();

        std::lock_guard<std::mutex> lock{cout_mutex};
        std::cout << "\n"
                  << (connected ? "✓ Successfully connected!" : "✗ Connection failed")
                  << "\nores-client> " << std::flush;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Connect exception: " << e.what();
        std::lock_guard<std::mutex> lock{cout_mutex};
        std::cout << "\n✗ Connection error: " << e.what() << "\nores-client> " << std::flush;
    }
}

void repl::process_disconnect() {
    if (!client_) {
        BOOST_LOG_SEV(lg, warn) << "No client instance.";
        return;
    }

    if (!client_->is_connected()) {
        BOOST_LOG_SEV(lg, debug) << "Already disconnected.";
        return;
    }

    client_->disconnect();
    BOOST_LOG_SEV(lg, info) << "Disconnected from server.";
}

boost::asio::awaitable<void> repl::process_get_currencies() {
    try {
        BOOST_LOG_SEV(lg, debug) << "Sending get currencies request.";

        risk::messaging::get_currencies_request request;
        auto request_payload = request.serialize();

        comms::protocol::frame request_frame(
            comms::protocol::message_type::get_currencies_request,
            0,
            std::move(request_payload));

        BOOST_LOG_SEV(lg, debug) << "Sending request frame.";

        auto response_result =
            co_await client_->send_request(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg, error) << "Request failed with error code: "
                                      << static_cast<int>(response_result.error());
            std::lock_guard<std::mutex> lock{cout_mutex};
            std::cout << "✗ Request failed\nores-client> " << std::flush;
            co_return;
        }

        BOOST_LOG_SEV(lg, debug) << "Deserializing response";

        auto response = risk::messaging::get_currencies_response::deserialize(
            response_result->payload());

        if (!response) {
            BOOST_LOG_SEV(lg, error) << "Failed to deserialize response: "
                                      << static_cast<int>(response.error());
            std::lock_guard<std::mutex> lock{cout_mutex};
            std::cout << "✗ Failed to parse response\nores-client> " << std::flush;
            co_return;
        }

        BOOST_LOG_SEV(lg, info) << "Successfully retrieved "
                                << response->currencies.size() << " currencies";

        std::lock_guard<std::mutex> lock{cout_mutex};
        std::cout << "\n" << response->currencies << "\nores-client> " << std::flush;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Get currencies exception: " << e.what();
        std::lock_guard<std::mutex> lock{cout_mutex};
        std::cout << "\n✗ Error: " << e.what() << "\nores-client> " << std::flush;
    }
}

void repl::start_io_thread() {
    BOOST_LOG_SEV(lg, info) << "Starting I/O thread";

    io_thread_ = std::make_unique<std::thread>([this]() {
        BOOST_LOG_SEV(lg, info) << "I/O thread started";
        try {
            io_ctx_->run();
            BOOST_LOG_SEV(lg, info) << "I/O thread ended normally";
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg, error) << "I/O thread exception: " << e.what();
        }
    });
}

void repl::stop_io_thread() {
    if (!io_thread_) {
        return;
    }

    BOOST_LOG_SEV(lg, info) << "Stopping I/O thread";

    work_guard_.reset();

    if (io_thread_->joinable()) {
        io_thread_->join();
    }

    io_thread_.reset();
    BOOST_LOG_SEV(lg, info) << "I/O thread stopped";
}

void repl::display_welcome() const {
    std::cout << "ORE Studio Client REPL" << std::endl;
    std::cout << "Type 'help' for available commands, 'exit' to quit" << std::endl;
    std::cout << std::endl;
}

}
