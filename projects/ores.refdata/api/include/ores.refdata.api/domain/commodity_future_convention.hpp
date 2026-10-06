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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a commodity future, its expiry schedule and the options written on it.
 *
 * Describes how ORE builds a commodity future: when it expires, which contract
 * months it lists, how its expiry anchors to a day of the month or a weekday, and
 * the schedule of the options written on it. Corresponds to the
 * <CommodityFuture> element in ORE conventions.xml. The id field is the natural
 * key (ORE <Id> element).
 *
 * Thirty of the element's thirty-four fields are modelled here, as forty columns,
 * because the element's nested structs are flattened rather than given tables of
 * their own.
 * AnchorDay becomes seven columns, OptionNthWeekday two more, and
 * OffPeakPowerIndexData four. The corpus uses all six of AnchorDay's variants,
 * so flattening it is what lets a commodity future round trip at all.
 *
 * Five files carry the element, in eight hundred and eight elements, and two of
 * them carried nothing else unmodelled when it landed.
 *
 * Four more fields hold lists, and each is written as a text column rather than
 * counted: AveragingData becomes eight columns, and ProhibitedExpiries,
 * FutureContinuationMappings and OptionContinuationMappings become one text
 * column each, holding comma-separated dates and from:to pairs. A column is a
 * weaker home than a table, and it is what keeps the field in the element's
 * round trip rather than excluding the file from it.
 */
struct commodity_future_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique commodity future identifier.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Frequency at which contracts are listed, as the canonical code the mapper stores.
     */
    std::string contract_frequency;

    /**
     * @brief Calendar the contract's expiry rolls on.
     */
    std::string calendar;

    /**
     * @brief Calendar the expiry date rolls on, when it differs from the contract calendar.
     */
    std::optional<std::string> expiry_calendar;

    /**
     * @brief Months between the contract month and its expiry.
     */
    std::optional<int> expiry_month_lag;

    /**
     * @brief The single contract month, for a future that lists only one.
     */
    std::optional<std::string> one_contract_month;

    /**
     * @brief Days between the expiry and the contract's start.
     */
    std::optional<int> offset_days;

    /**
     * @brief Business day convention the expiry rolls on.
     */
    std::optional<std::string> business_day_convention;

    /**
     * @brief Whether the offset is applied before the business day adjustment.
     */
    std::optional<bool> adjust_before_offset;

    /**
     * @brief Whether the contract settles on an average of prices.
     */
    std::optional<bool> is_averaging;

    /**
     * @brief Contract months the future lists, comma separated.
     */
    std::optional<std::string> valid_contract_months;

    /**
     * @brief Day of the month the contract anchors on.
     */
    std::optional<int> anchor_day_of_month;

    /**
     * @brief Calendar days before the anchor that the contract starts.
     */
    std::optional<int> anchor_calendar_days_before;

    /**
     * @brief Business days after the anchor that the contract starts.
     */
    std::optional<int> anchor_business_days_after;

    /**
     * @brief Position of the anchor's weekday within its month.
     */
    std::optional<int> anchor_nth_nth;

    /**
     * @brief Weekday the anchor falls on, when it is the nth of them.
     */
    std::optional<std::string> anchor_nth_weekday;

    /**
     * @brief Last weekday of the month the contract anchors on.
     */
    std::optional<std::string> anchor_last_weekday;

    /**
     * @brief Day of the week the contract anchors on.
     */
    std::optional<std::string> anchor_weekly_day_of_the_week;

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
     * @brief Hours in a delivery day, for a power future.
     */
    std::optional<int> hours_per_day;

    /**
     * @brief Index the off-peak hours price.
     */
    std::optional<std::string> off_peak_index;

    /**
     * @brief Index the peak hours price.
     */
    std::optional<std::string> peak_index;

    /**
     * @brief Hours of the day that count as off-peak.
     */
    std::optional<double> off_peak_hours;

    /**
     * @brief Calendar the peak hours are read on.
     */
    std::optional<std::string> peak_calendar;

    /**
     * @brief Name of the index the future references.
     */
    std::optional<std::string> index_name;

    /**
     * @brief Daylight savings time zone the contract follows.
     */
    std::optional<std::string> savings_time;

    /**
     * @brief Location the contract delivers to.
     */
    std::optional<std::string> delivery_location;

    /**
     * @brief Whether the contract includes the balance of the current month.
     */
    std::optional<bool> balance_of_the_month;

    /**
     * @brief Calendar the balance of the month prices on.
     */
    std::optional<std::string> balance_of_the_month_pricing_calendar;

    /**
     * @brief Convention of the future the option is written on.
     */
    std::optional<std::string> option_underlying_future_convention;

    /**
     * @brief Commodity the averaging data prices.
     */
    std::optional<std::string> averaging_commodity_name;

    /**
     * @brief Period the averaging data covers, as the canonical code the mapper stores.
     */
    std::optional<std::string> averaging_period;

    /**
     * @brief Calendar the averaged prices are read on.
     */
    std::optional<std::string> averaging_pricing_calendar;

    /**
     * @brief Conventions the averaging follows, as ORE spells them.
     */
    std::optional<std::string> averaging_conventions;

    /**
     * @brief Whether the averaging counts business days only.
     */
    std::optional<bool> averaging_use_business_days;

    /**
     * @brief Days the delivery rolls by.
     */
    std::optional<int> averaging_delivery_roll_days;

    /**
     * @brief Months between the future and the averaged month.
     */
    std::optional<int> averaging_future_month_offset;

    /**
     * @brief Days between the daily expiry and the averaged date.
     */
    std::optional<int> averaging_daily_expiry_offset;

    /**
     * @brief Expiry dates the future may not use, comma separated.
     */
    std::optional<std::string> prohibited_expiries;

    /**
     * @brief Mappings from one future contract to the next, as from:to pairs, comma separated.
     */
    std::optional<std::string> future_continuation_mappings;

    /**
     * @brief Mappings from one option contract to the next, as from:to pairs, comma separated.
     */
    std::optional<std::string> option_continuation_mappings;

    /**
     * @brief Username of the person who last modified this commodity future convention.
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
    friend bool operator==(const commodity_future_convention&,
                           const commodity_future_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_future_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_future_convention&) {
    return "ores.refdata.commodity_future_convention";
}

}

#endif
