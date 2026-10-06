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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_DEFINITION_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_DEFINITION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief Persistent template for a scheduled report.
 *
 * The persistent template for a report. Describes what to run, when to run it,
 * how to handle concurrent executions, and how much of the processing pipeline a
 * run executes. Type-specific configuration (e.g. risk parameters) lives in a
 * separate table keyed by report_definition_id.
 *
 * The run configuration is held here rather than in the environment so that a run
 * is reproducible from the database alone. pre_processing and post_processing
 * name, for each substitutable phase, whether the phase runs or is replaced by a
 * prepared substitute.
 *
 * Lifecycle is managed through the report_definition_lifecycle FSM machine.
 * fsm_state_id points to the current state in ores_dq_fsm_states_tbl.
 *
 * scheduler_job_id links to ores_scheduler_job_definitions_tbl.id and is set
 * by the scheduler service when the definition is activated (state: active).
 * It is cleared when the definition is suspended or archived.
 */
struct report_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this report definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique name for the report definition within a party.
     */
    std::string name;

    /**
     * @brief Party that owns this report definition.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Human-readable description of the report.
     */
    std::string description;

    /**
     * @brief Report type code (FK to ores_reporting_report_types_tbl).
     */
    std::string report_type;

    /**
     * @brief Current FSM state (FK to ores_dq_fsm_states_tbl). Null until first activation.
     */
    std::optional<boost::uuids::uuid> fsm_state_id;

    /**
     * @brief Validated cron expression driving report recurrence.
     */
    std::string schedule_expression;

    /**
     * @brief Concurrency policy code (FK to ores_reporting_concurrency_policies_tbl).
     */
    std::string concurrency_policy;

    /**
     * @brief Scheduler job UUID (FK to ores_scheduler_job_definitions_tbl). Present only when
     * status is active.
     */
    std::optional<boost::uuids::uuid> scheduler_job_id;

    /**
     * @brief The IAM run grant the definition's scheduled runs act under. Created when a person
     * schedules the definition, with their consent, and revoked when it is unscheduled. It is not a
     * credential: a step exchanges it for a run token. IAM owns the grant, so this column is not a
     * checked foreign key.
     */
    std::optional<boost::uuids::uuid> run_grant_id;

    /**
     * @brief What a run does to prepare the engine's input: execute generates it from the
     * definition's scope, substitute resolves the prepared archive named by prepared_input_key.
     */
    std::string pre_processing;

    /**
     * @brief Storage key of the prepared input archive that a substituting run resolves. Set only
     * when pre_processing is substitute.
     */
    std::string prepared_input_key;

    /**
     * @brief What a run does with the engine's results: execute ingests them into the result store,
     * ignore records only that the engine produced them.
     */
    std::string post_processing;

    /**
     * @brief Whether the report is an official one.
     *
     * An official report never reads a [[id:4AB0BC63-D73A-4FC3-B9AF-16C1BB90653F][sandbox]]: its
     * scope cannot name a sandbox portfolio or a virtual book, and the books it resolves never
     * include one. A definition defaults to official, so a report reads only official data unless
     * it is set up otherwise.
     */
    bool is_official = true;

    /**
     * @brief Username of the person who last modified this report definition.
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
    friend bool operator==(const report_definition&, const report_definition&) = default;
};

/**
 * @brief Dispatch-key identifier for report_definition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_definition&) {
    return "ores.reporting.report_definition";
}

}

#endif
