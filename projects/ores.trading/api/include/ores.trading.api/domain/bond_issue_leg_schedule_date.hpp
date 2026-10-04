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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_LEG_SCHEDULE_DATE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_LEG_SCHEDULE_DATE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One date of a bond issue leg schedule's date list, keyed to the schedule entry that holds
 * it.
 *
 * One row per date of a bond issue leg schedule's date list, family-owned
 * by the schedule row that holds it.
 *
 * The ORE schema states a schedule as a choice, and this table holds the
 * date arm: the explicit list a document writes out. The rule arm writes
 * no row here, because a calendar expands its dates at read time rather
 * than the document stating them.
 *
 * The owning schedule's key reaches this table whole. A schedule entry
 * is keyed by the issue, the leg, the entry's role and its ordinal, so
 * those four columns lead the key here and the date's own ordinal follows
 * them.
 *
 * A leg states four schedules, and one of them, the payment dates of a
 * leg, is a bare date list with no rule arm at all. Such a list is a
 * schedule of kind dates whose scalars stay unset, so it writes here
 * without a table of its own.
 */
struct bond_issue_leg_schedule_date final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The bond issue whose leg states the owning schedule.
     *
     * The issue row carries the party. The date rows are family-owned and ride the issue's scope.
     */
    boost::uuids::uuid issue_id;

    /**
     * @brief Ordinal of the leg within the issue's leg list, counting from one.
     */
    int leg_number;

    /**
     * @brief Which of the leg's schedules the owning row is.
     */
    std::string schedule_role;

    /**
     * @brief Ordinal of the owning schedule entry within its leg's list for that role, counting
     * from one.
     *
     * The parent's sequence_number reaches this table under this name, so a date stays bound to the
     * entry that holds it when one leg states several entries under one role.
     */
    int schedule_sequence_number;

    /**
     * @brief Ordinal of this date within the schedule's list.
     *
     * The schema declares the list unbounded, and a document's order is the order it stated. The
     * ordinal preserves that order, so export re-emits the dates as the document held them.
     */
    int sequence_number;

    /**
     * @brief The date (ISO 8601 date string).
     */
    std::chrono::year_month_day schedule_date;

    /**
     * @brief Username of the person who last modified this bond issue leg schedule date.
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
    friend bool operator==(const bond_issue_leg_schedule_date&,
                           const bond_issue_leg_schedule_date&) = default;
};

/**
 * @brief Dispatch-key identifier for bond_issue_leg_schedule_date, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_issue_leg_schedule_date&) {
    return "ores.trading.bond_issue_leg_schedule_date";
}

}

#endif
