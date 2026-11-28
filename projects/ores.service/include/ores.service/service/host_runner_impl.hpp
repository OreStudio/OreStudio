/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_SERVICE_SERVICE_HOST_RUNNER_IMPL_HPP
#define ORES_SERVICE_SERVICE_HOST_RUNNER_IMPL_HPP

#include "ores.platform/credentials/credential_store.hpp"
#include "ores.telemetry.core/log/lifecycle_manager.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.vault.client/client/service_password.hpp"
#include <boost/exception/diagnostic_information.hpp>
#include <cstdlib>

namespace ores::service::service {

namespace detail {

/**
 * @brief Logs the run before the application starts.
 */
template <typename Config>
void log_host_command(ores::logging::logger_t& lg,
                      const std::vector<std::string>& args,
                      const Config& cfg,
                      const host_runner_options& opts) {
    using namespace ores::logging;

    if (opts.on_before_log)
        opts.on_before_log();

    ORES_LOG_SEV(lg, info) << "Command line arguments: " << args;
    ORES_LOG_SEV(lg, debug) << "Configuration: " << cfg;
}

/**
 * @brief Takes this unit's database password from the vault, when one was placed.
 *
 * The first secret a workload resolves is its own database password, and it has
 * to be resolved before the pool opens, which is before any token can exist. A
 * deployment states the unit's consumer and the secret to ask for; the platform
 * places the key pair; the vault seals a challenge to the public half. See
 * =lib/doc/knowledge/security/secure_introduction_of_a_workload_to_the_vault.org=.
 *
 * A configuration that states neither name is left alone, so a checkout, a test
 * run and a tool that links this runner still start from their own options. A
 * named unit whose vault refuses keeps the configured password and logs the
 * refusal: the fleet boots, and the operator sees why it did not use the vault.
 * The fallback goes when the environment file stops carrying a password to fall
 * back to.
 */
/**
 * @brief Takes this unit's database password from the credential the deployment
 * placed, for the units that cannot ask the vault.
 *
 * Two services have to exist before the vault answers: the vault, and IAM, which
 * the vault fetches its token keys from. The environment file no longer states
 * their password, so the deployment places it as a credential and this reads it.
 * A unit with no such credential and a password in its configuration is left
 * alone, which is what a checkout and a test run look like.
 */
template <typename Config>
void take_database_password_from_credential(Config& cfg, ores::logging::logger_t& lg) {
    using namespace ores::logging;

    if constexpr (requires { cfg.database; }) {
        if (!cfg.database.password.empty())
            return;

        try {
            const auto placed = ores::platform::credentials::credential_store::read("db-password");
            cfg.database.password.assign(placed.begin(), placed.end());
            ORES_LOG_SEV(lg, info) << "Took the database password from the credential placed "
                                      "for this unit";
        } catch (const std::exception&) {
            // No credential: the unit's configuration is expected to carry a
            // password, and if it does not the pool says so.
        }
    }
}

template <typename Config>
void take_database_password_from_vault(Config& cfg, ores::logging::logger_t& lg) {
    using namespace ores::logging;

    if constexpr (requires {
                      cfg.database;
                      cfg.nats;
                  }) {
        try {
            const auto placed = ores::vault::client::placed_credential_from_environment();
            if (!placed)
                return;

            const auto resolved = ores::vault::client::resolve_placed_password(cfg.nats, *placed);
            if (!resolved)
                return;

            cfg.database.password = *resolved;
            ORES_LOG_SEV(lg, info)
                << "Took the database password from the vault as " << placed->consumer;
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg, error) << "The vault did not answer for " << cfg.database.user
                                    << ", so the configured password stands: " << e.what();
        }
    }
}

/**
 * @brief Logs a failure from the application and leaves the rethrow to the caller.
 *
 * A boost::exception carries diagnostics that what() alone loses, so it is
 * logged in full.
 */
inline void log_host_failure(ores::logging::logger_t& lg,
                             const std::exception& e,
                             std::string_view failure_message) {
    using namespace ores::logging;

    const auto* const be(dynamic_cast<const boost::exception* const>(&e));
    if (be != nullptr) {
        using boost::diagnostic_information;
        ORES_LOG_SEV(lg, error) << "Error: " << diagnostic_information(*be);
    } else {
        ORES_LOG_SEV(lg, error) << "Error: " << e.what();
    }
    ORES_LOG_SEV(lg, error) << failure_message;
}

}

template <typename Parser, typename Application>
boost::asio::awaitable<int> run_host_async(const std::vector<std::string>& args,
                                           std::ostream& std_output,
                                           std::ostream& error_output,
                                           boost::asio::io_context& io_ctx,
                                           ores::logging::logger_t& lg,
                                           host_runner_options opts) {
    using namespace ores::logging;
    using ores::telemetry::log::lifecycle_manager;

    Parser p;
    const auto ocfg(p.parse(args, std_output, error_output));

    if (!ocfg)
        co_return EXIT_SUCCESS;

    // A copy, because the database password may be replaced below and the
    // application is handed the resolved one rather than the file's.
    auto cfg(*ocfg);
    lifecycle_manager lm(cfg.logging);

    detail::take_database_password_from_credential(cfg, lg);
    detail::take_database_password_from_vault(cfg, lg);
    detail::log_host_command(lg, args, cfg, opts);

    try {
        Application app;
        co_await app.run(io_ctx, cfg);

        if (opts.on_success)
            opts.on_success();

        co_return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        detail::log_host_failure(lg, e, opts.failure_message);
        throw;
    }
}

template <typename Parser, typename EarlyExit, typename Runner>
int run_host_sync(const std::vector<std::string>& args,
                  std::ostream& std_output,
                  std::ostream& error_output,
                  ores::logging::logger_t& lg,
                  EarlyExit early_exit,
                  Runner run_application,
                  host_runner_options opts) {
    using namespace ores::logging;
    using ores::telemetry::log::lifecycle_manager;

    Parser p;
    const auto ocfg(p.parse(args, std_output, error_output));

    if (!ocfg)
        return EXIT_SUCCESS;

    const auto& cfg(*ocfg);
    lifecycle_manager lm(cfg.logging);

    detail::log_host_command(lg, args, cfg, opts);

    if (const auto exit_code = early_exit(cfg))
        return *exit_code;

    try {
        run_application(cfg);

        if (opts.on_success)
            opts.on_success();

        return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        detail::log_host_failure(lg, e, opts.failure_message);
        throw;
    }
}

}

#endif
