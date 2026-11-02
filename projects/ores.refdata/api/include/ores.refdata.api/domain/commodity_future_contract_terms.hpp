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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONTRACT_TERMS_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_CONTRACT_TERMS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The terms of the futures contract itself: how it is listed, when it expires and what it
 * delivers.
 *
 * The terms that state the futures contract itself, as ORE's
 * ContractFrequency, Calendar, the expiry calendar, month lag and offset, the
 * contract months, the business day convention, the averaging flag, the hours
 * per day, the index name, the savings time, the delivery location, the balance
 * of the month fields, the prohibited expiries and the future continuation
 * mappings. It is a part because its fields hold one invariant: each describes
 * the listed contract, and ORE requires the frequency and the calendar of every
 * commodity future. So every convention of the kind fills this part once. The
 * anchor day, the option expiry, the off-peak index and the averaging data beside
 * it are optional, and each is a part of its own.
 */
struct commodity_future_contract_terms final {
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
     * @brief Hours in a delivery day, for a power future.
     */
    std::optional<int> hours_per_day;

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
     * @brief Expiry dates the future may not use, comma separated.
     */
    std::optional<std::string> prohibited_expiries;

    /**
     * @brief Mappings from one future contract to the next, as from:to pairs, comma separated.
     */
    std::optional<std::string> future_continuation_mappings;

    /**
     * @brief Username of the person who last modified this commodity future contract terms.
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
    friend bool operator==(const commodity_future_contract_terms&,
                           const commodity_future_contract_terms&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_future_contract_terms, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_future_contract_terms&) {
    return "ores.refdata.commodity_future_contract_terms";
}

}

#endif
