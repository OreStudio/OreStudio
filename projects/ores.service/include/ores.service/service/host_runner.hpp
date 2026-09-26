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
#ifndef ORES_SERVICE_SERVICE_HOST_RUNNER_HPP
#define ORES_SERVICE_SERVICE_HOST_RUNNER_HPP

#include "ores.logging/make_logger.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include <functional>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace ores::service::service {

/**
 * @brief Optional hooks/overrides for the host runners, defaulted to match the
 * plain host shape. Lets tools like ores.http.server and ores.shell customise
 * the startup/shutdown log lines and the failure message without forking the
 * whole envelope.
 */
struct host_runner_options final {
    std::function<void()> on_before_log;
    std::function<void()> on_success;
    std::string_view failure_message = "Failed to execute command.";
};

/**
 * @brief Runs the standard async host lifecycle shared by every NATS-based
 * service/tool: parse args -> (if configuration present) init logging -> log
 * args/configuration -> construct and run the application -> on exception,
 * log diagnostic information and rethrow.
 *
 * Extracted from what used to be ~50 near-identical lines hand-duplicated in
 * every service's host.cpp (see task_unify_application_cpp_shape.org) --
 * two services (ores.ore.service, ores.workflow.service) had independently
 * drifted from the canonical shape, silently skipping the failure log line
 * for non-boost exceptions; using this shared implementation removes that
 * class of drift by construction rather than requiring a one-off fix.
 *
 * @tparam Parser Default-constructible; parse(args, std_output, error_output)
 * returns an optional configuration object with a streamable `logging` field
 * usable to construct ores::telemetry::log::lifecycle_manager.
 * @tparam Application Default-constructible; run(io_ctx, cfg) returns
 * boost::asio::awaitable<void>.
 * @param lg The calling service's own logger (kept per-service so log lines
 * carry the right logger_name).
 * @param opts By value, deliberately: boost::asio::awaitable coroutines are
 * lazily started, so the calling host::execute() has already returned (and
 * destroyed any temporary/local it passed) before this body ever runs --
 * a by-reference opts parameter is a dangling-reference crash waiting to
 * happen the moment the default `{}` (or a caller's local host_runner_options)
 * goes out of scope. args/std_output/error_output/io_ctx/lg stay by
 * reference safely only because they refer to objects that outlive the
 * whole process (main()'s own locals, static loggers), not this call frame.
 */
template <typename Parser, typename Application>
boost::asio::awaitable<int> run_host_async(const std::vector<std::string>& args,
                                           std::ostream& std_output,
                                           std::ostream& error_output,
                                           boost::asio::io_context& io_ctx,
                                           ores::logging::logger_t& lg,
                                           host_runner_options opts = {});

/**
 * @brief Runs the standard sync host lifecycle for a tool that cannot use the
 * io_context: parse args -> (if configuration present) init logging -> log
 * args/configuration -> optional early exit (for DB-free commands that must
 * not go through the generic failure log) -> construct and run the
 * application -> on exception, log diagnostic information and rethrow.
 *
 * Deliberately does *not* attempt to unify application construction: the
 * shell's constructor takes NATS/login/script options, so callers supply a
 * run_application callable that owns construction.
 *
 * @tparam Parser Default-constructible; parse(args, std_output, error_output)
 * returns an optional configuration object with a streamable `logging` field
 * usable to construct ores::telemetry::log::lifecycle_manager.
 * @tparam EarlyExit Callable(const cfg&) -> std::optional<int>; returning a
 * value short-circuits before the try/catch below with that exit code;
 * returning std::nullopt proceeds to run_application.
 * @tparam Runner Callable(const cfg&) -> void; constructs and runs the
 * application.
 * @param lg The calling tool's own logger (kept per-tool so log lines carry
 * the right logger_name).
 */
template <typename Parser, typename EarlyExit, typename Runner>
int run_host_sync(const std::vector<std::string>& args,
                  std::ostream& std_output,
                  std::ostream& error_output,
                  ores::logging::logger_t& lg,
                  EarlyExit early_exit,
                  Runner run_application,
                  host_runner_options opts = {});

}

#include "ores.service/service/host_runner_impl.hpp"

#endif
