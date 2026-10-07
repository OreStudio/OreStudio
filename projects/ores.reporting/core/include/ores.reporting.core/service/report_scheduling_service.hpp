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

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.scheduler.api/messaging/job_definition_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/uuid.hpp>
#include <expected>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Bridges the reporting service and the scheduler service.
 *
 * Handles two responsibilities:
 *  1. Reconciliation: brings the scheduler's report jobs in line with the
 *     definitions. Called once after the service starts and on every
 *     definition change.
 *  2. On-demand scheduling: called by the schedule/unschedule NATS handlers
 *     to create or remove scheduler jobs for specific definitions.
 *
 * Scheduler jobs are the reporting service's own, so it puts and deletes them
 * with its own client. Run grants are a person's consent, so it asks IAM for
 * them with the client that relays the person's token.
 */
class ORES_REPORTING_CORE_EXPORT report_scheduling_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_scheduling_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @param svc_nats    The service's own client, for scheduler jobs.
     * @param person_nats The client that relays the person's token, for run
     *                    grants. Reconciliation acts for no person and omits it.
     */
    report_scheduling_service(
        context ctx,
        ores::nats::service::nats_client svc_nats,
        std::optional<ores::nats::service::nats_client> person_nats = std::nullopt);

    /**
     * @brief Brings the scheduler's report jobs in line with the definitions.
     *
     * A report job is kept only when a definition records both the job's id
     * and the grant its runs act under. Any other report job is removed: it is
     * either a schedule that did not finish, or a job whose runs no person
     * consented to. A definition that holds a grant but has lost its job is
     * put back, because the grant is the consent; a definition that records a
     * job with no grant keeps no job and stops naming one. Reconciliation
     * never creates a schedule, because a job no person asked for would fire
     * runs no one consented to. Safe to call on every restart.
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
    std::expected<bool, std::string> schedule_one(const domain::report_definition& def,
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
    std::expected<bool, std::string> unschedule_one(const domain::report_definition& def,
                                                    const std::string& actor);

private:
    /**
     * @brief Build and send a put_job_definition_request to the scheduler.
     *
     * @param def      The report definition to schedule.
     * @param job_id   Pre-allocated UUID to use as the job's primary key.
     * @return success or unexpected(error message).
     */
    std::expected<void, std::string> send_schedule_request(const domain::report_definition& def,
                                                           const boost::uuids::uuid& job_id);

    /**
     * @brief Build a scheduler job change for a report definition.
     *
     * Used by both schedule_one and the reconciliation restore path. The write
     * carries the job's own fields; the caller supplies the intent, because
     * the change reason differs between the two paths.
     *
     * @param def    The report definition.
     * @param job_id Pre-allocated UUID to use as the job's primary key.
     * @return A populated change, or nullopt if the cron is invalid.
     */
    std::optional<ores::scheduler::messaging::job_definition_change>
    build_job_change(const domain::report_definition& def, const boost::uuids::uuid& job_id);

    /**
     * @brief Deletes a scheduler job with the service's own client.
     */
    std::expected<void, std::string> send_delete_request(const boost::uuids::uuid& job_id,
                                                         const std::string& commentary);

    /**
     * @brief Asks IAM, on behalf of the person scheduling, for the grant the
     * definition's runs act under, and returns its id.
     */
    std::expected<boost::uuids::uuid, std::string> grant_runs(const domain::report_definition& def);

    /**
     * @brief Asks IAM, on behalf of the person unscheduling, to revoke a grant.
     */
    std::expected<void, std::string> revoke_runs(const boost::uuids::uuid& grant_id);

    context ctx_;
    ores::nats::service::nats_client svc_nats_;
    std::optional<ores::nats::service::nats_client> person_nats_;
};

}

#endif
