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
#ifndef ORES_REFDATA_API_DOMAIN_INFLATION_SWAP_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_INFLATION_SWAP_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for an inflation swap, where a leg pays a fixed rate against a zero-coupon
 * inflation index.
 *
 * Describes how ORE builds an inflation swap: the calendars and conventions its
 * legs roll on, the day count it accrues on, the zero-coupon inflation index it
 * references, and how the index's observations are lagged and adjusted.
 * Corresponds to the <InflationSwap> element in ORE conventions.xml. The id field
 * is the natural key (ORE <Id> element).
 *
 * Ten fields are required and the rest are optional. The corpus sets all ten in
 * all one hundred and one elements. Of the optional fields it sets only
 * PublicationRoll, in two elements.
 *
 * The element also carries PublicationSchedule, which ORE types as a schedule
 * of rule blocks, date blocks and derived schedule groups. Each block is written
 * as one text column in a stated form: blocks are separated by semicolons and a
 * block's fields by pipes, which is enough to keep every value the binding holds
 * without a table of its own. Two shipped files set a schedule.
 */
struct inflation_swap_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique inflation swap identifier.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Calendar the fixed leg's fixings are read on.
     */
    std::string fix_calendar;

    /**
     * @brief Business day convention of the fixed leg, as the canonical code the mapper stores.
     */
    std::string fix_convention;

    /**
     * @brief Day count fraction the swap accrues on.
     */
    std::string day_count_fraction;

    /**
     * @brief Zero-coupon inflation index the swap references.
     */
    std::string index;

    /**
     * @brief Whether the index is interpolated between publication dates.
     */
    bool interpolated = false;

    /**
     * @brief Lag between an observation date and the index value used for it, as ORE's period code.
     */
    std::string observation_lag;

    /**
     * @brief Whether an observation date that falls on a holiday moves to the next business day.
     */
    bool adjust_inflation_observation_dates = false;

    /**
     * @brief Calendar the index's observation dates roll on.
     */
    std::string inflation_calendar;

    /**
     * @brief Business day convention of the index's observation dates.
     */
    std::string inflation_convention;

    /**
     * @brief How a payment date moves when it falls between the index's observation and its
     * publication, as the canonical code the mapper stores.
     */
    std::optional<std::string> publication_roll;

    /**
     * @brief Delay between the swap's start and its first observation, as ORE's period code.
     */
    std::optional<std::string> start_delay;

    /**
     * @brief Business day convention applied to the start delay.
     */
    std::optional<std::string> start_delay_convention;

    /**
     * @brief Name of the publication schedule, when ORE gives it one.
     */
    std::optional<std::string> publication_schedule_name;

    /**
     * @brief The schedule's rule blocks, one per semicolon, each a pipe-separated record of start
     * date, end date, adjust-end-date flag, tenor, calendar, convention, term convention, rule,
     * end-of-month, end-of-month convention, first date, last date, remove-first flag and
     * remove-last flag. An empty field is absent and a field of ~ is present but empty, because ORE
     * writes an element the document leaves blank and the export has to write it back.
     */
    std::optional<std::string> publication_schedule_rules;

    /**
     * @brief The schedule's date blocks, one per semicolon, each a pipe-separated record of
     * calendar, convention, tenor, end-of-month, include-duplicates and a comma-separated date
     * list.
     */
    std::optional<std::string> publication_schedule_dates;

    /**
     * @brief The schedule's derived groups, one per semicolon, each a pipe-separated pair of the
     * derived schedule and the derived flag.
     */
    std::optional<std::string> publication_schedule_derived_groups;

    /**
     * @brief Username of the person who last modified this inflation swap convention.
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
    friend bool operator==(const inflation_swap_convention&,
                           const inflation_swap_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for inflation_swap_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const inflation_swap_convention&) {
    return "ores.refdata.inflation_swap_convention";
}

}

#endif
