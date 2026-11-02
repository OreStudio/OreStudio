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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_OPTION_EXPIRY_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_OPTION_EXPIRY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The expiry schedule of the options written on a commodity future.
 *
 * The expiry schedule of the options written on a commodity future, as ORE's
 * Option* fields: the month lag, the contract frequency, the offset, the
 * calendar and business days before, the expiry day, the nth weekday, the last
 * weekday, the weekly day and the business day convention, with the future
 * convention an option names as its underlying and the option continuation
 * mappings. It is a part because its fields hold one invariant: each applies only
 * when the convention states options on the future. A convention with no
 * options fills none of it, so it fills this part at most once.
 */
struct commodity_future_option_expiry final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The surrogate key of this part row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The central convention row this part belongs to. One row per convention, so the
     * generated table adds the unique index on the live rows of (tenant_id, convention_id): a
     * convention fills this form or it does not, and never two of it.
     */
    boost::uuids::uuid convention_id;

    /**
     * @brief The party that owns the convention this part belongs to. Carried so row level security
     * can isolate the part without a join.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Months between the option's contract month and its expiry.
     */
    std::optional<int> option_expiry_month_lag;

    /**
     * @brief Frequency at which the underlying future's contracts are listed.
     */
    std::optional<std::string> option_contract_frequency;

    /**
     * @brief Offset in days between the option's expiry and its underlying's.
     */
    std::optional<int> option_expiry_offset;

    /**
     * @brief Calendar days before its anchor that the option expires.
     */
    std::optional<int> option_calendar_days_before;

    /**
     * @brief Minimum business days before its anchor that the option expires.
     */
    std::optional<int> option_min_business_days_before;

    /**
     * @brief Day of the month the option expires on.
     */
    std::optional<int> option_expiry_day;

    /**
     * @brief Position of the option's expiry weekday within its month.
     */
    std::optional<int> option_nth_nth;

    /**
     * @brief Weekday the option expires on, when it is the nth of them.
     */
    std::optional<std::string> option_nth_weekday;

    /**
     * @brief Last weekday of the month the option expires on.
     */
    std::optional<std::string> option_expiry_last_weekday_of_month;

    /**
     * @brief Day of the week the option expires on.
     */
    std::optional<std::string> option_expiry_weekly_day_of_the_week;

    /**
     * @brief Business day convention the option's expiry rolls on.
     */
    std::optional<std::string> option_business_day_convention;

    /**
     * @brief Convention of the future the option is written on.
     */
    std::optional<std::string> option_underlying_future_convention;

    /**
     * @brief Mappings from one option contract to the next, as from:to pairs, comma separated.
     */
    std::optional<std::string> option_continuation_mappings;

    /**
     * @brief Username of the person who last modified this commodity future option expiry.
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
    friend bool operator==(const commodity_future_option_expiry&,
                           const commodity_future_option_expiry&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_future_option_expiry, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_future_option_expiry&) {
    return "ores.refdata.commodity_future_option_expiry";
}

}

#endif
