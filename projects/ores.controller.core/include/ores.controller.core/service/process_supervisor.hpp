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
#ifndef ORES_CONTROLLER_CORE_SERVICE_PROCESS_SUPERVISOR_HPP
#define ORES_CONTROLLER_CORE_SERVICE_PROCESS_SUPERVISOR_HPP

#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>
#include <filesystem>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/process/v2/process.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.controller.api/domain/service_definition.hpp"

namespace ores::controller::service {

/**
 * @brief Manages OS-level lifecycle of all supervised services.
 *
 * Reads service definitions from the database, spawns child processes
 * via boost::process::v2, monitors them for unexpected exits, and
 * applies the configured restart policy. Each replica gets a distinct
 * --log-replica-index so its log file is uniquely named.
 *
 * Usage in the bootstrap sequence:
 *   1. co_spawn start_all() as a detached background coroutine. It reads
 *      the service dependency graph from the DB, starts services in
 *      topological order, and waits for each service to be ready before
 *      launching its dependents (IAM starts first; all other services wait
 *      for it).
 *   2. Connect to NATS. service::run() fetches the JWKS key from IAM with
 *      exponential backoff — it will succeed once start_all() has IAM up.
 *   3. Register NATS handlers and begin serving requests.
 *
 * NATS handlers call request_launch / request_stop / request_restart
 * from a synchronous callback; these post the async work to the
 * io_context so the handler can reply immediately.
 */
class process_supervisor {
private:
    inline static std::string_view logger_name =
        "ores.controller.service.process_supervisor";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Default args template used when service_definition.args_template is empty.
    // {nats_tls_args} expands to "--nats-tls-ca … --nats-tls-cert … --nats-tls-key …"
    // when mTLS is configured, or to an empty string otherwise.
    static constexpr std::string_view default_args_template =
        "--log-enabled "
        "--log-level {log_level} "
        "--log-directory {log_dir} "
        "--log-replica-index {replica_index} "
        "--nats-url {nats_url} "
        "--nats-subject-prefix {nats_prefix} "
        "{nats_tls_args}";

    using process_key = std::pair<std::string, int>; // (service_name, replica_index)

    struct process_entry {
        std::optional<boost::process::v2::process> proc;
        api::domain::service_definition def;
        int replica_index = 0;
        int restart_count = 0;
        bool stop_requested = false; // true when stop() was intentionally called
    };

public:
    process_supervisor(boost::asio::io_context& ioc,
        std::filesystem::path bin_dir,
        ores::nats::config::nats_options nats,
        std::string log_level,
        ores::database::context db_ctx);

    // -------------------------------------------------------------------------
    // Bulk start / stop

    /**
     * @brief Reads all enabled service definitions and their dependencies,
     * then launches services in topological order.
     *
     * For each service that has dependents, waits until "Service ready"
     * appears in its log file before launching services that depend on it.
     * Services with no dependents are launched and left to run without waiting.
     *
     * Safe to co_spawn as a detached background coroutine — the caller's
     * NATS/JWKS setup proceeds concurrently.
     */
    boost::asio::awaitable<void> start_all();

    /**
     * @brief Gracefully stops all supervised processes.
     *
     * Sends SIGTERM to every running replica, waits up to 10 seconds for
     * them to exit cleanly, then sends SIGKILL to any that are still
     * running. Should be called when the controller itself is shutting down.
     */
    boost::asio::awaitable<void> stop_all();

    // -------------------------------------------------------------------------
    // Per-service operations (called from NATS handlers)

    /**
     * @brief Posts an async launch of the given service replica.
     * Safe to call from a synchronous NATS handler callback.
     */
    void request_launch(const std::string& service_name, int replica_index);

    /**
     * @brief Posts an async stop (SIGTERM) of the given service replica.
     * Safe to call from a synchronous NATS handler callback.
     */
    void request_stop(const std::string& service_name, int replica_index);

    /**
     * @brief Posts an async restart (stop + launch) of the given replica.
     */
    void request_restart(const std::string& service_name, int replica_index);

private:
    boost::asio::awaitable<void> do_launch(std::string service_name,
        int replica_index);
    boost::asio::awaitable<void> do_stop(std::string service_name,
        int replica_index);
    boost::asio::awaitable<void> monitor_process(
        std::shared_ptr<process_entry> entry);
    boost::asio::awaitable<bool> wait_for_log_ready(
        std::filesystem::path log_path, std::chrono::seconds timeout,
        std::uintmax_t start_offset = 0);

    std::vector<std::string> build_args(const api::domain::service_definition& def,
        int replica_index) const;
    std::filesystem::path log_path_for(const std::string& service_name,
        int replica_index) const;
    std::filesystem::path pid_path_for(const std::string& service_name,
        int replica_index) const;
    void write_pid_file(const std::string& service_name, int replica_index,
        boost::process::v2::pid_type pid) const;
    void remove_pid_file(const std::string& service_name,
        int replica_index) const;

    boost::asio::io_context& ioc_;
    std::filesystem::path bin_dir_;
    ores::nats::config::nats_options nats_;
    std::string log_level_;
    ores::database::context db_ctx_;
    std::map<process_key, std::shared_ptr<process_entry>> processes_;
};

} // namespace ores::controller::service

#endif
