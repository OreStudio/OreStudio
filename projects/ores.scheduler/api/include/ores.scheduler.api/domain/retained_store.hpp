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
#ifndef ORES_SCHEDULER_API_DOMAIN_RETAINED_STORE_HPP
#define ORES_SCHEDULER_API_DOMAIN_RETAINED_STORE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::scheduler::domain {

/**
 * @brief One store the installation keeps, described in the words a person reads.
 *
 * Every store the installation keeps is a row here, before anything else knows
 * about it. The row is what lets a person read "Calendar dates" instead of
 * ores_refdata_calendar_dates_tbl, and what lets the sweep, the screen and the
 * alerts agree on the set of stores without any of them carrying a list of tables.
 *
 * The catalogue is not a policy: it states what the store is and the bound it ships
 * with, and a policy states what is applied now. That is why a store exists here
 * whether or not a policy names it, and why the bound is one of three kinds rather
 * than always an action and a period. A store bounded by its content keying needs
 * no policy, and a store whose bound cannot be expressed as an age — the calendar
 * days behind the horizon — states that it has none rather than carrying a default
 * that would delete what pricing still needs.
 *
 * The rows describe the installation's own stores, so the platform authors them and
 * the system tenant holds them: the classification that governs published rows
 * makes the platform the author here, as it does for the policies those rows
 * describe. The column is not null for that reason.
 */
struct retained_store final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The store's code, in the dotted form the components use.
     *
     * Examples: 'refdata.calendar_dates', 'reporting.report_cashflows', 'compute.node_scratch'.
     */
    std::string code;

    /**
     * @brief The name a person reads on the screen, in place of the physical target.
     *
     * Examples: 'Calendar dates', 'Compute node scratch'.
     */
    std::string display_name;

    /**
     * @brief What the store holds, in a sentence, so a person knows what would be lost.
     */
    std::string description;

    /**
     * @brief The component that writes the store, and therefore the one to ask about it.
     *
     * Examples: 'refdata', 'reporting', 'marketdata', 'compute'.
     */
    std::string owner_component;

    /**
     * @brief Whether the platform or a single tenant owns the rows. It decides the audience of an
     * alert about the store: the platform's stores are the system administrator's, and a tenant's
     * are that tenant's.
     */
    std::string scope = std::string("platform");

    /**
     * @brief The shape of the target: a chunked table, an ordinary table, an object-store key
     * prefix, or a directory on a compute host.
     */
    std::string target_kind;

    /**
     * @brief The table name, the key prefix, or the directory the store lives in.
     */
    std::string target;

    /**
     * @brief The column a deletion compares against the period. Null where nothing compares a
     * column: an object prefix or a directory expires by its own age.
     */
    std::optional<std::string> time_column;

    /**
     * @brief How the store's size is read: counts of rows, of objects, or of files.
     */
    std::string measure_kind;

    /**
     * @brief The kind of bound the store ships with: a policy with an action and a period, a bound
     * by content keying that needs no policy, or no bound at all where none can be expressed as an
     * age.
     */
    std::string bound_kind = std::string("policy");

    /**
     * @brief What the default policy does to the target past its period, when the bound is a
     * policy.
     */
    std::optional<std::string> default_action;

    /**
     * @brief The default period, in days, when the bound is a policy.
     */
    std::optional<int> default_retention_days;

    /**
     * @brief The hours between two applications of the default policy.
     */
    int default_cadence_hours = 24;

    /**
     * @brief Why the store keeps what it keeps for that period, stated once so the policy the
     * installation creates carries it.
     */
    std::optional<std::string> default_reason;

    /**
     * @brief Username of the person who last modified this retained store.
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
    friend bool operator==(const retained_store&, const retained_store&) = default;
};

/**
 * @brief Dispatch-key identifier for retained_store, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const retained_store&) {
    return "ores.scheduler.retained_store";
}

}

#endif
