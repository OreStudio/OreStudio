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
application::run_client(const std::optional<config::client_options>& ocfg) const {
    if (!ocfg.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "No client configuration found.";
        co_return;
    }

    const auto& cfg(ocfg.value());
    BOOST_LOG_SEV(lg, info) << "Starting client REPL for "
                             << cfg.host << ":" << cfg.port;

    try {
        // Create client configuration
        comms::client_config client_cfg;
        client_cfg.host = cfg.host;
        client_cfg.port = cfg.port;
        client_cfg.client_identifier = cfg.client_identifier;
        client_cfg.verify_certificate = cfg.verify_certificate;

        // Create client (shared_ptr for use in lambdas)
        auto cli = std::make_shared<comms::client>(
            std::move(client_cfg),
            co_await boost::cobalt::this_coro::executor);

        // Create CLI root menu
        auto rootMenu = std::make_unique<::cli::Menu>("ores-client");

        // Add HANDSHAKE command
        // Note: Since cobalt::promise requires a specific executor context and the cli library
        // expects synchronous handlers, we'll need to run the handshake in a blocking manner
        rootMenu->Insert(
            "HANDSHAKE",
            [](std::ostream& out) {
                out << "HANDSHAKE command not yet fully implemented" << std::endl;
                out << "Async/await integration with REPL needs further work" << std::endl;
            },
            "Perform handshake with the server");

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
