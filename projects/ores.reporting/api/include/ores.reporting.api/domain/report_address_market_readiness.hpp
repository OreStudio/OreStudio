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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_ADDRESS_MARKET_READINESS_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_ADDRESS_MARKET_READINESS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief Which oresmd addresses a run needed, and which of them its market answered.
 *
 * +#+updated: 2026-11-26
 *
 * One row per oresmd address the run needs, written before the engine sees any
 * work, whether or not the market answers it.
 *
 * This is the grain that answers why the market is short. A party that has to buy
 * the data needs to know whether it is missing a series or missing a point in one
 * it holds, and those are different purchases, so the row names the series that
 * answers the address when there is one and the cause when there is not.
 *
 * An address several trades name is one row, because the question is about the
 * address and not about the trade that asked for it.
 */
struct report_address_market_readiness final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this readiness row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run this row states the readiness of.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The address the run needed, as the trading component resolved it.
     */
    std::string oresmd_uri;

    /**
     * @brief The series that answers the address, absent when nothing holds it. It is written as
     * the commit points out the series, not the address alone, because a provider may hold two
     * series under one address and only one of them is the one the run read.
     */
    std::optional<boost::uuids::uuid> series_id;

    /**
     * @brief Why the market did not answer the address, empty when it did. The codes are no_series,
     * when nothing holds the address, and no_point, when a series holds it but carries no point at
     * or before the run's as-of. A cause no read can state is not invented here; the list grows
     * when a read can state more.
     */
    std::string gap_cause;

    /**
     * @brief Username of the person who last modified this address readiness.
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
    friend bool operator==(const report_address_market_readiness&,
                           const report_address_market_readiness&) = default;
};

/**
 * @brief Dispatch-key identifier for report_address_market_readiness, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_address_market_readiness&) {
    return "ores.reporting.report_address_market_readiness";
}

}

#endif
