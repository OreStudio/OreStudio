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
#include "ores.shell/app/host.hpp"

#include <cstdlib>
#include <memory>
#include <boost/exception/diagnostic_information.hpp>
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.telemetry/domain/resource.hpp"
#include "ores.telemetry/domain/telemetry_context.hpp"
#include "ores.telemetry/exporting/hybrid_log_exporter.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.shell/app/application.hpp"
#include "ores.shell/config/parser.hpp"

namespace ores::shell::app {

using namespace ores::telemetry::log;
using ores::shell::config::parser;
using ores::telemetry::log::lifecycle_manager;

int host::execute(const std::vector<std::string>& args,
    std::ostream& std_output, std::ostream& error_output) {
    /*
     * Create the configuration from command line options.
     */
    parser p;
    const auto ocfg(p.parse(args, std_output, error_output));

    /*
     * If we have no configuration, then there is nothing to do. This can only
     * happen if the user requested some valid options such as help or version;
     * any errors at the command line level are treated as exceptions, and all
     * other cases must result in a configuration object.
     */
    if (!ocfg)
        return EXIT_SUCCESS;

    /*
     * Since we have a configuration, we can now attempt to initialise the
     * logging subsystem.
     */
    const auto& cfg(*ocfg);
    lifecycle_manager lm(cfg.logging);

    /*
     * Set up telemetry export if enabled.
     */
    std::shared_ptr<telemetry::exporting::hybrid_log_exporter> exporter;
    std::optional<telemetry::domain::telemetry_context> telemetry_ctx;
    if (cfg.telemetry) {
        const auto& tcfg(*cfg.telemetry);

        /*
         * Create the resource that describes this service instance.
         */
        auto resource = std::make_shared<telemetry::domain::resource>(
            telemetry::domain::resource::from_environment(
                tcfg.service_name, tcfg.service_version));

        /*
         * Create a root telemetry context for this session.
         */
        telemetry_ctx = telemetry::domain::telemetry_context::create_root(resource);

        /*
         * Compute the full path to the telemetry output file.
         */
        auto path = tcfg.output_directory / tcfg.output_file;

        /*
         * Create the hybrid exporter for JSON Lines output with optional streaming.
         *
         * When streaming is enabled, records are batched and sent to the server
         * in addition to being written to the local file. The send callback
         * needs access to the client session for sending, which requires
         * integration with the application's connection management.
         *
         * TODO: Wire up send_callback to client_session for server streaming.
         * This requires refactoring client_session to be created earlier and
         * shared between the application and the exporter.
         */
        telemetry::exporting::send_records_callback send_callback = nullptr;
        exporter = std::make_shared<telemetry::exporting::hybrid_log_exporter>(
            path, tcfg, std::move(send_callback));

        /*
         * Add the telemetry sink to capture all log records.
         */
        lm.add_telemetry_sink(resource, [exporter](auto rec) {
            exporter->export_record(std::move(rec));
        });
        BOOST_LOG_SEV(lg(), info)
            << "Telemetry export enabled, writing to: " << path;
        if (tcfg.streaming_enabled) {
            BOOST_LOG_SEV(lg(), info)
                << "Telemetry streaming enabled (batch_size="
                << tcfg.batch_size << ", flush_interval="
                << tcfg.flush_interval.count() << "s)";
        }
        BOOST_LOG_SEV(lg(), info)
            << "Telemetry context created - trace_id: "
            << telemetry_ctx->get_trace_id().to_hex();
    }

    /*
     * Log the configuration and command line arguments.
     */
    BOOST_LOG_SEV(lg(), info) << "Command line arguments: " << args;
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;

    /*
     * Execute the application.
     */
    try {
        ores::shell::app::application app(cfg.connection, cfg.login, telemetry_ctx);
        app.run();
        return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        /*
         * Try to dump useful, but less user-friendly information by
         * interrogating the exception. Note that we must catch by
         * std::exception and cast the boost exception; if we were to catch
         * boost exception, we would not have access to the what() method and
         * thus could not provide the exception message to the console.
         */
        const auto *const be(dynamic_cast<const boost::exception* const>(&e));
        if (be == nullptr)
            throw;

        using boost::diagnostic_information;
        BOOST_LOG_SEV(lg(), error) << "Error: " << diagnostic_information(*be);
        BOOST_LOG_SEV(lg(), error) << "Failed to execute command.";
        throw;
    }
}

}
