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
#ifndef ORES_SHELL_APP_COMMANDS_COMPUTE_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_COMPUTE_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Non-interactive equivalent of ores.qt's compute "Upload Engines"
 * flow -- publishes a compute engine package (app/version/platform/binary)
 * so the system is ready to dispatch jobs to it.
 *
 * Intended as a manual step run once after ACME's own "core" party/org
 * provisioning completes (not folded into "provision party --source acme"),
 * since packaging/publishing an engine binary is a release-time concern
 * distinct from provisioning a tenant.
 */
class compute_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.compute_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register compute-related commands.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Publish a compute engine package: compute publish-package
     * <app_name> <engine_version> <platform_code> [--file <path>]
     * [--wrapper-version <v>] [--min-ram-mb <n>] [--http-base-url <url>]
     *
     * Uploads the package to the canonical per-triplet storage key,
     * verifies its SHA256 against the server's own hash of the received
     * bytes, then upserts the app/app_version/app_version_platform rows --
     * reusing an existing app (matched by name) or app_version (matched by
     * app + engine_version + wrapper_version) if one already exists, so
     * running this command again for the same triplet is idempotent.
     */
    static void process_publish_package(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);

    /**
     * @brief List compute apps: compute list-apps
     *
     * Renders the app catalogue (id, name, description) as a table.
     */
    static void process_list_apps(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief List compute app versions: compute list-app-versions
     *
     * Renders the registered app versions (wrapper/engine version,
     * min RAM) as a table.
     */
    static void process_list_app_versions(std::ostream& out,
                                          ores::nats::service::nats_client& session);

    /**
     * @brief List compute platforms: compute list-platforms
     *
     * Renders the known platform triplets (code, display name, OS,
     * CPU arch) as a table.
     */
    static void process_list_platforms(std::ostream& out,
                                       ores::nats::service::nats_client& session);

    /**
     * @brief List compute hosts: compute list-hosts
     *
     * Renders the registered grid nodes (id, display name, hardware,
     * last RPC time, credit) as a table.
     */
    static void process_list_hosts(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief List compute batches: compute list-batches
     *
     * Renders the batches (id, external ref, status) as a table.
     */
    static void process_list_batches(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Create a compute batch: compute add-batch <external_ref>
     * <job_count> [--smoke]
     *
     * Creates a batch row with status "open". The job count is session
     * knowledge -- the batch row stores no count -- and is passed again
     * at dispatch. --smoke bounds the count to the config range
     * (10..20), which is what the grid smoke-test script passes.
     */
    static void process_add_batch(std::ostream& out,
                                  ores::nats::service::nats_client& session,
                                  const std::vector<std::string>& args);

    /**
     * @brief Dispatch a batch's jobs: compute dispatch-batch
     * <external_ref> <job_count> <app_version_id> <input_dir>
     *
     * Packs the input directory and uploads it once as the batch's
     * shared input bundle, marks the batch "dispatched", then saves one
     * workunit per job (each save dispatches to the grid: the backend
     * creates the result rows and publishes the JetStream assignments).
     */
    static void process_dispatch_batch(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief List workunits: compute list-workunits [--batch <external_ref>]
     *
     * Renders the workunits as a table, optionally filtered to one
     * batch (matched by external ref).
     */
    static void process_list_workunits(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief List results: compute list-results [--batch <external_ref>]
     *
     * Renders the submitted results as a table, optionally filtered to
     * one batch (matched by external ref via its workunits).
     */
    static void process_list_results(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief Grid telemetry: compute grid-stats [--watch <external_ref>]
     * [--smoke] [--timeout <seconds>]
     *
     * Renders the grid summary and per-node rows. With --watch, polls
     * every 10 seconds until every workunit of the batch (matched by
     * external ref) has a canonical result, then renders the final
     * snapshot. With --watch --smoke, additionally asserts the smoke
     * guarantees on the drained batch: every result has outcome
     * Success and every host online at the drain transition has at
     * least one result in the batch; the command fails if either
     * check does not hold.
     */
    static void process_grid_stats(std::ostream& out,
                                   ores::nats::service::nats_client& session,
                                   const std::vector<std::string>& args);

    /**
     * @brief Remove a host row: compute delete-host <host_id>
     *
     * Refuses to delete a host that is online (heartbeated within the
     * 90s heartbeat timeout); an offline host row is removed.
     */
    static void process_delete_host(std::ostream& out,
                                    ores::nats::service::nats_client& session,
                                    const std::vector<std::string>& args);

    /**
     * @brief Download a job's input bundle: compute download-input
     * <workunit_id> <dest_dir>
     *
     * Fetches the batch's shared input bundle referenced by the
     * workunit and unpacks the tar.gz into the destination directory.
     */
    static void process_download_input(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief Download a result's output bundle: compute download-output
     * <result_id> <dest_dir>
     *
     * Fetches the output bundle the node uploaded with the result and
     * unpacks the tar.gz into the destination directory.
     */
    static void process_download_output(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);
};

}

#endif
