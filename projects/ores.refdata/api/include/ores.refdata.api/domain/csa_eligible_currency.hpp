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
#ifndef ORES_REFDATA_API_DOMAIN_CSA_ELIGIBLE_CURRENCY_HPP
#define ORES_REFDATA_API_DOMAIN_CSA_ELIGIBLE_CURRENCY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A currency a CSA accepts as collateral.
 *
 * One row per currency a [[id:29A8E20A-0F02-4D54-830E-8DDD668E86C0][CSA]] accepts as collateral.
 * ORE lists them under the CSA's EligibleCollaterals, in order; the position keeps that order so a
 * CSA written back to ORE lists them as it read them.
 */
struct csa_eligible_currency final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this row.
     *
     * Surrogate key for the eligible currency record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The CSA that accepts the currency.
     *
     * References the CSAs table.
     */
    boost::uuids::uuid csa_id;

    /**
     * @brief The ISO code of the eligible currency.
     *
     * A CSA lists a currency once. Not checked against the currencies table: like the currency
     * columns of the instrument models, it holds what the ORE document states.
     */
    std::string currency_code;

    /**
     * @brief The currency's place in ORE's list.
     *
     * Zero-based; two currencies of one CSA never share a position.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this CSA eligible currency.
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
    friend bool operator==(const csa_eligible_currency&, const csa_eligible_currency&) = default;
};

/**
 * @brief Dispatch-key identifier for csa_eligible_currency, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const csa_eligible_currency&) {
    return "ores.refdata.csa_eligible_currency";
}

}

#endif
