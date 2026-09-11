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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_SCHEDULE_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_SCHEDULE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One schedule entry an instrument's owner states, either as a rule block or as a date list,
 * keyed to the instrument, the owner, the schedule's role and the entry's ordinal.
 *
 * One row per schedule entry an instrument's owner states, keyed to the
 * instrument, the owner that states it, the schedule's role and the
 * entry's ordinal within that role.
 *
 * The ORE schema states a schedule as a choice: a rule block that a
 * calendar expands, or an explicit list of dates. The generated code
 * mirrors the choice as two lists on bond_schedule_data, and the
 * mapping container carries both. This table holds the rule arm's
 * scalars and the schedule_kind column records which arm the document
 * chose, so a reader rebuilds the arm it read rather than guessing. The
 * date arm's rows land in the keyed child table.
 *
 * Both lists are unbounded and a document interleaves their entries, so
 * the role alone does not identify a row. sequence_number is the
 * entry's ordinal within its owner's list for that role, counting from
 * one, and it preserves the order the document stated.
 *
 * A bond leg states four schedules: its own schedule, its payment
 * schedule, and, when the leg is floating, the fixing and reset
 * schedules. The role column names which one, so one table holds all
 * four. The column also carries the schedules that hang off an option
 * block and off a total return swap, so the table is keyed by owner
 * rather than by leg.
 *
 * The nine bond tables carry no schedule column, and the org models of
 * the family record this table as the destination. It is keyed by the
 * instrument rather than by a bond row, so any family whose product
 * states a schedule writes here without a new table.
 *
 * Every scalar the schema declares optional is nullable here and an
 * std::optional in C++, so a member the document states and the row
 * cannot hold stays distinguishable from one the document omits.
 */
struct instrument_schedule final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument whose owner states this schedule.
     *
     * The instrument row carries the trade, the workspace and the party. The schedule rows are
     * family-owned and ride the instrument's scope, so no workspace column rides them.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Which list of the instrument states this schedule: bond, trs_funding, repo,
     * ascot_swap, option or trs.
     *
     * The first four name a leg list. option is the exercise schedule an option block states, and
     * trs is the return schedule a total return swap states. Neither is a leg, so the column names
     * an owner rather than a leg.
     */
    std::string owner_role;

    /**
     * @brief Ordinal of the owner within its list, counting from one.
     *
     * The schema declares the bond's leg list unbounded, and a bond states one leg per coupon. The
     * number is the owner's position in the document, so a reader reassembles the owners in the
     * order the document held them. An owner that is not a list member holds one row and takes the
     * number one.
     */
    int owner_number;

    /**
     * @brief Which of the owner's schedules this row is.
     *
     * The container's member names are the values: schedule, payment_schedule, fixing_schedule and
     * reset_schedule. The first two sit on the leg, the last two on its floating rate block.
     */
    std::string schedule_role;

    /**
     * @brief Ordinal of this entry within its owner's list for the schedule's role, counting from
     * one.
     *
     * The schema states a schedule as an unbounded choice of rules and dates, and a document
     * interleaves the two arms. One owner can therefore state several entries under one role, and
     * the role alone does not identify a row. The ordinal preserves the order the document stated.
     */
    int sequence_number;

    /**
     * @brief Which arm of the schema's choice the document stated: rules or dates.
     *
     * The arm decides which members are meaningful. A rules row carries the rule block's scalars
     * and its dates are derived, so it has no child rows; a dates row carries the date list in the
     * child table and states no rule scalars.
     */
    std::string schedule_kind;

    /**
     * @brief First date of the rule block.
     *
     * The schema requires this member on a rule block and states no equivalent on a date list, so
     * the column is nullable and a dates row leaves it unset.
     */
    std::optional<std::string> start_date;

    /**
     * @brief Last date of the rule block, when the document states one.
     */
    std::optional<std::string> end_date;

    /**
     * @brief The document's spelling of the flag that pulls the end date back to the previous month
     * end.
     *
     * The schema types this member as its own bool, which enumerates thirteen spellings including
     * the empty one. The column holds the spelling the document chose rather than a decoded
     * boolean, so export re-emits the same text.
     */
    std::optional<std::string> adjust_end_date_to_previous_month_end;

    /**
     * @brief Tenor the rule block expands, or the date list's own tenor.
     *
     * Both arms state this member and the schema requires it on neither alone, so one nullable
     * column serves both.
     */
    std::optional<std::string> tenor;

    /**
     * @brief Calendar the schedule's dates are adjusted against.
     *
     * Both arms state this member.
     */
    std::optional<std::string> calendar;

    /**
     * @brief Business day convention the schedule's dates are adjusted by.
     *
     * Both arms state this member.
     */
    std::optional<std::string> convention;

    /**
     * @brief Business day convention applied to the rule block's final date.
     */
    std::optional<std::string> term_convention;

    /**
     * @brief The rule block's date generation rule.
     */
    std::optional<std::string> rule;

    /**
     * @brief The document's spelling of the flag that keeps generated dates at month ends.
     *
     * The schema types this member as its own bool, as it does
     * adjust_end_date_to_previous_month_end, so the column holds the spelling.
     */
    std::optional<std::string> end_of_month;

    /**
     * @brief Business day convention applied when the end-of-month flag is on.
     */
    std::optional<std::string> end_of_month_convention;

    /**
     * @brief First date of the rule block, stated outright.
     */
    std::optional<std::string> first_date;

    /**
     * @brief Last date of the rule block, stated outright.
     */
    std::optional<std::string> last_date;

    /**
     * @brief True when the rule block drops its first date.
     *
     * The schema types this member as the XML boolean, not as its own bool, so the column is a real
     * boolean and carries no spelling.
     */
    std::optional<bool> remove_first_date;

    /**
     * @brief True when the rule block drops its last date.
     */
    std::optional<bool> remove_last_date;

    /**
     * @brief The document's spelling of the flag that keeps repeated dates in the list.
     *
     * The schema types this member as its own bool, so the column holds the spelling.
     */
    std::optional<std::string> include_duplicate_dates;

    /**
     * @brief Username of the person who last modified this instrument schedule.
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
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for instrument_schedule, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_schedule&) {
    return "ores.trading.instrument_schedule";
}

}

#endif
