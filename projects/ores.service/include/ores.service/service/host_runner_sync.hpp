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
#ifndef ORES_SERVICE_SERVICE_HOST_RUNNER_SYNC_HPP
#define ORES_SERVICE_SERVICE_HOST_RUNNER_SYNC_HPP

#include "ores.logging/make_logger.hpp"
#include <functional>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace ores::service::service {

/**
 * @brief Optional hooks/overrides for run_host_sync, defaulted to match the
 * plain sync-tool shape.
 */
struct host_runner_sync_options final {
    std::function<void()> on_before_log;
    std::function<void()> on_success;
    std::string_view failure_message = "Failed to execute command.";
};

/**
 * @brief Runs the standard sync host lifecycle shared by ores.cli and
 * ores.shell: parse args -> (if configuration present) init logging -> log
 * args/configuration -> optional early exit (for DB-free commands that must
 * not go through the generic failure log) -> construct and run the
 * application -> on exception, log diagnostic information and rethrow.
 *
 * Extracted from what used to be ~90% near-identical lines hand-duplicated
 * across ores.cli/host.cpp and ores.shell/host.cpp (see
 * task_investigate_cli_shell_host_divergence.org: sync execution is
 * load-bearing for both -- neither belongs on run_host_async's io_context --
 * but their own bodies still shared this much structure with each other).
 *
 * Deliberately does *not* attempt to unify application construction: neither
 * tool's constructor is uniform with the other (ores.cli's takes an output
 * stream + database options, ores.shell's takes NATS/login/script options),
 * so callers supply a run_application callable that owns construction.
 *
 * @tparam Parser Default-constructible; parse(args, std_output, error_output)
 * returns an optional configuration object with a streamable `logging` field
 * usable to construct ores::telemetry::log::lifecycle_manager.
 * @tparam EarlyExit Callable(const cfg&) -> std::optional<int>; returning a
 * value short-circuits before the try/catch below with that exit code (used
 * by ores.cli's DB-free ore_roundtrip branch, which has its own try/catch and
 * output formatting); returning std::nullopt proceeds to run_application.
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
                  host_runner_sync_options opts = {});

}

#include "ores.service/service/host_runner_sync_impl.hpp"

#endif
