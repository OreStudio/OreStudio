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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_INSTANCE_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_INSTANCE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A single execution of a report definition.
 *
 * A single execution of a report_definition. Created automatically when the
 * scheduler fires a trigger for an active definition.
 *
 * Lifecycle is managed through the report_instance_lifecycle FSM machine.
 * fsm_state_id points to the current state in ores_dq_fsm_states_tbl.
 *
 * started_at is NULL when the instance is cancelled or skipped before execution
 * begins. completed_at is NULL while running or in a terminal-before-start state.
 */
struct report_instance final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this report instance.
     */
    boost::uuids::uuid id;

    /**
     * @brief Human-readable name copied from the report definition at creation time.
     */
    std::string name;

    /**
     * @brief Description copied from the report definition at creation time.
     */
    std::string description;

    /**
     * @brief Party that owns this report instance (inherited from the definition).
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The report definition that produced this instance.
     *
     * The column is filterable so that a caller asking for the runs of the definitions a party owns
     * sends their ids, rather than paging the party's whole run history and discarding the rest.
     * The filter record then carries definition_id and definition_id_one_of.
     */
    boost::uuids::uuid definition_id;

    /**
     * @brief What this instance does: production prepares the input, computes it on the grid and
     * records the results, and package prepares the input and stops, so that its packages can be
     * inspected and run locally. Refers to ores_reporting_report_instance_kinds_tbl. The scheduler
     * creates production instances only, the concurrency policy of the definition counts them only,
     * and a list shows them unless the caller asks for another kind.
     */
    std::string kind = "production";

    /**
     * @brief The IAM run grant that this run acts under, copied from the definition whatever
     * admitted the run: an on-demand trigger and a scheduled one reach the same admission. The
     * instance records the grant id so that a later change to the report's consent cannot change
     * the authority of a run that is already in flight. IAM owns the grant, so this column is not a
     * checked foreign key.
     */
    std::optional<boost::uuids::uuid> run_grant_id;

    /**
     * @brief Current FSM state (FK to ores_dq_fsm_states_tbl).
     */
    std::optional<boost::uuids::uuid> fsm_state_id;

    /**
     * @brief The trigger that admitted this run. Every instance has one: a run a person asks for
     * and a run a schedule asks for are admitted the same way, and the database enforces that this
     * names a trigger. The generated instance names its own uuid as the trigger, so the trigger it
     * points at is derived from the instance rather than minted beside it, and a fixture that
     * writes one can write the trigger row it names from the instance alone.
     */
    boost::uuids::uuid trigger_id;

    /**
     * @brief Execution log or error message.
     */
    std::string output_message;

    /**
     * @brief Storage key of the run's output archive, as the compute result named it. Set only once
     * the run's results have been retrieved.
     */
    std::string output_storage_key;

    /**
     * @brief When execution began. NULL if cancelled before starting. The admission service writes
     * it when the run is admitted, so a caller editing the instance never states it.
     */
    std::optional<std::chrono::system_clock::time_point> started_at;

    /**
     * @brief When execution completed. NULL while running or before start. The service that drives
     * the run writes it, so a caller editing the instance never states it.
     */
    std::optional<std::chrono::system_clock::time_point> completed_at;

    /**
     * @brief The date the run actually priced at. It is set once the packaging step has chosen the
     * date, which is the date the run document asked for when the gathered market data carries it,
     * and the last date that data carries when it does not. NULL until then, and NULL for a run
     * that never reached the packaging step.
     */
    std::optional<std::chrono::system_clock::time_point> priced_at;

    /**
     * @brief Why the run priced at the date it did, in the words the packaging step wrote. It is
     * empty when the run priced at the date it asked for, and states the date it asked for and the
     * date the gathered data ends at when the two differ.
     */
    std::string price_note;

    /**
     * @brief The engine run settings this run read, copied from the definition's binding when the
     * run was admitted. The engine run settings are a configuration of their own, owned by the
     * reporting component, so this column is not a checked foreign key. A run whose definition
     * binds none has no row and names none.
     */
    std::optional<boost::uuids::uuid> engine_settings_id;

    /**
     * @brief The version of the engine run settings the run read, so a run keeps the settings it
     * ran with whatever a later change makes official. A proposal that waits moves no version, so
     * this number names the settings a person may reproduce the numbers from.
     */
    std::optional<int> engine_settings_version;

    /**
     * @brief Username of the person who last modified this report instance.
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
    friend bool operator==(const report_instance&, const report_instance&) = default;
};

/**
 * @brief Dispatch-key identifier for report_instance, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_instance&) {
    return "ores.reporting.report_instance";
}

}

#endif
