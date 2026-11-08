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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_OFF_PEAK_INDEX_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FUTURE_OFF_PEAK_INDEX_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The peak and off-peak indices a power future's price is made of.
 *
 * The peak and off-peak indices a power future's price is made of, as ORE's
 * OffPeakPowerIndexData: the off-peak index, the peak index, the hours of the
 * day that count as off-peak, and the calendar the peak hours are read on. It is a
 * part because its fields hold one invariant: ORE requires all four together, so
 * the part makes each column required. Only a power future states it, so a
 * convention fills this part at most once.
 */
struct commodity_future_off_peak_index final {
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
     * @brief Index the off-peak hours price.
     */
    std::string off_peak_index;

    /**
     * @brief Index the peak hours price.
     */
    std::string peak_index;

    /**
     * @brief Hours of the day that count as off-peak.
     */
    double off_peak_hours{};

    /**
     * @brief Calendar the peak hours are read on.
     */
    std::string peak_calendar;

    /**
     * @brief Username of the person who last modified this commodity future off peak index.
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
    friend bool operator==(const commodity_future_off_peak_index&,
                           const commodity_future_off_peak_index&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_future_off_peak_index, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_future_off_peak_index&) {
    return "ores.refdata.commodity_future_off_peak_index";
}

}

#endif
