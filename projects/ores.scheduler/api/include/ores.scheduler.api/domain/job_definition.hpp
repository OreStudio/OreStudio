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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SCHEDULER_API_DOMAIN_JOB_DEFINITION_HPP
#define ORES_SCHEDULER_API_DOMAIN_JOB_DEFINITION_HPP

#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::scheduler::domain {

/**
 * @brief Persistent plan for a recurring or one-off scheduled job.
 *
 * A job the scheduler fires on a cron expression. The row carries the schedule
 * (schedule_expression), what to run when it fires, and whether it is active.
 * action_type selects the behaviour: execute_sql runs the SQL in command,
 * and nats_publish publishes the subject and body carried in action_payload.
 *
 * The table is bi-temporal and audited (see
 * projects/ores.sql/create/scheduler/scheduler_job_definitions_create.sql): it
 * carries version, the four audit columns and the valid_from/valid_to pair
 * with the GIST exclusion and the delete rule, so the model takes the ordinary
 * audited shape and needs no shape flag.
 *
 * tenant_id is nullable. A job may belong to no tenant, because the scheduler
 * fires system jobs from a NULL-tenant row: the MQ statistics scrape and the
 * compute stale-result reaper are both such rows. The model binds
 * uuid-identified-lookup for its UUID surrogate key, its tenant scope and its
 * standard presentation tier, and states nullable_tenant_id itself, which that
 * profile leaves to the model.
 *
 * job_name is the natural key and is unique within its tenant; id is the
 * surrogate. Uniqueness is what the component's upsert path relies on: a job that
 * arrives under an existing name updates that row in place instead of adding a
 * second one.
 *
 * The component's operational views — the global job-instance list and the live
 * scheduler status — are operations rather than entity verbs, and are modelled
 * in ores.scheduler.scheduling_operations.
 */
struct job_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::optional<utility::uuid::tenant_id> tenant_id;

    /**
     * @brief UUID primary key for the job definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique name for the job within its tenant. The scheduler seed writes
     * ores.mq.metrics_scrape and the compute seed writes compute.v1.reap.stale_results; both are
     * system jobs, so both carry a NULL tenant and neither collides with a tenant's own job of the
     * same name.
     */
    std::string job_name;

    /**
     * @brief Optional party scope for this job. NULL for a tenant-scoped job and for a system job.
     */
    std::optional<boost::uuids::uuid> party_id;

    /**
     * @brief Human-readable description of the job.
     */
    std::string description;

    /**
     * @brief SQL command to execute when action_type is execute_sql.
     */
    std::string command;

    /**
     * @brief Cron expression defining the schedule. It is the component's own validated
     * cron_expression type, so an invalid expression cannot enter the domain.
     */
    domain::cron_expression schedule_expression;

    /**
     * @brief Execution mode: execute_sql or nats_publish.
     */
    std::string action_type = "execute_sql";

    /**
     * @brief Payload for the nats_publish action type. The scheduler seed writes an empty object;
     * the compute seed writes {"subject":"compute.v1.work.reap"}.
     */
    std::string action_payload = "{}";

    /**
     * @brief Whether the job fires. A paused job keeps its row and its history.
     */
    bool is_active = true;

    /**
     * @brief Username of the person who last modified this job definition.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const job_definition&, const job_definition&) = default;
};

/**
 * @brief Dispatch-key identifier for job_definition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const job_definition&) {
    return "ores.scheduler.job_definition";
}

}

#endif
