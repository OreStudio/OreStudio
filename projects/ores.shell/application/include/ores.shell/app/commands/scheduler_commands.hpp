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
#ifndef ORES_SHELL_APP_COMMANDS_SCHEDULER_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SCHEDULER_COMMANDS_HPP

#include "ores.nats/service/nats_client.hpp"
#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief The scheduler commands: what is scheduled, and what fires.
 *
 * The component's own service answers the protocol, so these commands are a
 * reader and a writer of it rather than a second implementation:
 *
 * - @c jobs — the definitions the caller can see, with their cron.
 * - @c schedule — create a definition, which is what arms a job.
 * - @c remove — delete a definition by name.
 * - @c instances — the executions the scheduler recorded.
 * - @c status — each job's schedule, its last run and its next firing.
 * - @c watch — subscribe to the execution events and print them as they
 *   arrive, which is the only way to see a job fire from the shell.
 *
 * The commands are hand-written rather than generated: the generated command
 * units project one entity onto one submenu, and the two things this flow
 * needs that no generated unit carries are an operation read (the instance
 * list and the status) and a subscription that outlives one request.
 */
class scheduler_commands {
public:
    /**
     * @brief Register the scheduler submenu on the shell's root menu.
     */
    static void register_commands(cli::Menu& root, ores::nats::service::nats_client& session);

    /**
     * @brief List the job definitions the caller can see.
     */
    static void process_jobs(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Create a job definition, which arms the job.
     *
     * @param args Positional: job name, cron expression. Flags: --action,
     *             --payload, --description, --inactive.
     */
    static void process_schedule(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief Delete a job definition by its name.
     */
    static void process_remove(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::string& job_name);

    /**
     * @brief List the executions the scheduler recorded.
     *
     * The filter is applied to the page the service returns, and the page is
     * the newest executions first, so a narrow --limit can exclude an older
     * execution of the job being asked about. Raise --limit to widen it.
     *
     * @param args Flags: --job, --limit.
     */
    static void process_instances(std::ostream& out,
                                  ores::nats::service::nats_client& session,
                                  const std::vector<std::string>& args);

    /**
     * @brief Show each job's schedule, last run and next firing.
     */
    static void process_status(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Print the execution events that arrive while the command runs.
     *
     * @param args Flags: --seconds, the time to listen for.
     */
    static void process_watch(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);
};

} // namespace ores::shell::app::commands

#endif
