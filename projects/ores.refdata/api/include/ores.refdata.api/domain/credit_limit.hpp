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
#ifndef ORES_REFDATA_API_DOMAIN_CREDIT_LIMIT_HPP
#define ORES_REFDATA_API_DOMAIN_CREDIT_LIMIT_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A counterparty credit line that restricts which trades one netting set may offset.
 *
 * A credit limit is the amount of credit the firm allows one counterparty,
 * set by one legal entity of the firm. It is the dimension that splits a
 * [[id:6AADB1A7-1229-4D10-8951-58446788957F][netting agreement]]'s trades into separate
 * [[id:83C9697A-0D6D-42A2-9917-6C68268B5404][netting sets]]: where one agreement covers trades
 * under several limits, netting is restricted to the trades within one limit (see
 * [[id:5F12A37F-9F26-4B49-BAAF-1BAA7B2BB94F][Netting sets]]).
 *
 * The limit is a credit fact, not a legal one, so it stands apart from the
 * agreement: one agreement may cover several limits, and one limit may
 * cover trades under several agreements. A netting set names both, and a
 * pinned key holds the set's counterparty and party to the limit's, as it
 * does for the agreement.
 */
struct credit_limit final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this credit limit.
     *
     * Surrogate key for the credit limit record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The reference the firm gives the limit.
     *
     * Unique within the legal entity that set it, as an agreement number is.
     */
    std::string code;

    /**
     * @brief The legal entity of the firm that set the limit.
     *
     * References the parties table. Fixed for the limit's life, so the netting sets that pin their
     * party to it can never be left on another party.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The counterparty the limit is set against.
     *
     * References the counterparties table. Fixed for the limit's life.
     */
    boost::uuids::uuid counterparty_id;

    /**
     * @brief The credit the firm allows the counterparty under this limit.
     *
     * Not negative. Absent when the limit is a bucket the firm has not quantified.
     */
    std::optional<ores::utility::decimal::decimal> limit_amount;

    /**
     * @brief Optional description of the limit.
     */
    std::optional<std::string> description;

    /**
     * @brief Username of the person who last modified this credit limit.
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
    friend bool operator==(const credit_limit&, const credit_limit&) = default;
};

/**
 * @brief Dispatch-key identifier for credit_limit, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const credit_limit&) {
    return "ores.refdata.credit_limit";
}

}

#endif
