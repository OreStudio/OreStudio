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
#ifndef ORES_REPORTING_SERVICE_REPORT_SCHEDULING_SERVICE_HPP
#define ORES_REPORTING_SERVICE_REPORT_SCHEDULING_SERVICE_HPP

#include <string>
#include <expected>
#include <optional>
#include <vector>
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.scheduler.api/domain/job_definition.hpp"

namespace ores::reporting::service {

/**
 * @brief Bridges the reporting service and the scheduler service.
 *
 * Handles two responsibilities:
 *  1. Startup reconciliation: ensures every active report definition has a
 *     corresponding scheduler job. Called once after the service starts.
 *  2. On-demand scheduling: called by the schedule/unschedule NATS handlers
 *     to create or remove scheduler jobs for specific definitions.
 */
class report_scheduling_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_scheduling_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    report_scheduling_service(context ctx,
        ores::nats::service::nats_client svc_nats);

    /**
     * @brief Ensures all report definitions have a scheduler job.
     *
     * Loads all definitions where scheduler_job_id IS NULL across all tenants
     * and creates a scheduler job for each one. Safe to call on every restart.
     */
    boost::asio::awaitable<void> reconcile();

    /**
     * @brief Schedule one definition by creating a scheduler job.
     *
     * Creates a nats_publish job in the scheduler and stores the returned
     * scheduler_job_id on the definition. Skips definitions already scheduled.
     *
     * @param def      The definition to schedule (must have a valid id and
     *                 schedule_expression).
     * @param actor    Authenticated username from this service's request context.
     *                 Stamped as modified_by on the updated report definition and
     *                 forwarded in the scheduler request payload.
     * @return true  — job was created.
     *         false — definition already had a scheduler_job_id (no-op).
     *         unexpected(msg) — scheduling failed; msg contains the error.
     */
    std::expected<bool, std::string>
    schedule_one(const domain::report_definition& def,
        const std::string& actor);

    /**
     * @brief Unschedule one definition by removing its scheduler job.
     *
     * Deletes the scheduler job and clears scheduler_job_id on the definition.
     * Skips definitions that are not currently scheduled.
     *
     * @param def      The definition to unschedule.
     * @param actor    Authenticated username. See schedule_one.
     * @return true  — job was removed.
     *         false — definition was not scheduled (no-op).
     *         unexpected(msg) — unscheduling failed; msg contains the error.
     */
    std::expected<bool, std::string>
    unschedule_one(const domain::report_definition& def,
        const std::string& actor);

private:
    /**
     * @brief Build and send a schedule_job_request to the scheduler.
     *
     * @param def      The report definition to schedule.
     * @param job_id   Pre-allocated UUID to use as the job's primary key.
     * @param actor    Username for audit fields on the job definition.
     * @return success or unexpected(error message).
     */
    std::expected<void, std::string>
    send_schedule_request(const domain::report_definition& def,
        const boost::uuids::uuid& job_id,
        const std::string& actor);

    /**
     * @brief Build a scheduler job_definition for a report definition.
     *
     * Used by both schedule_one and the batch reconciliation path.
     *
     * @param def    The report definition.
     * @param job_id Pre-allocated UUID to use as the job's primary key.
     * @param actor  Username stamped as modified_by on the job definition.
     * @return A populated job_definition, or nullopt if the cron is invalid.
     */
    std::optional<ores::scheduler::domain::job_definition>
    build_job_definition(const domain::report_definition& def,
        const boost::uuids::uuid& job_id,
        const std::string& actor);

    context ctx_;
    ores::nats::service::nats_client svc_nats_;
};

}

#endif
