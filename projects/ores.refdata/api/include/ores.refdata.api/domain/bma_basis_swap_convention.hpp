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
#ifndef ORES_REFDATA_API_DOMAIN_BMA_BASIS_SWAP_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_BMA_BASIS_SWAP_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a basis swap between a BMA municipal index and an IBOR or overnight index.
 *
 * Describes how ORE builds a basis swap whose two legs reference a BMA municipal
 * index and an IBOR or overnight index, and how each leg's payments are
 * scheduled. Corresponds to the <BMABasisSwap> element in ORE conventions.xml.
 * The id field is the natural key (ORE <Id> element).
 *
 * Three fields are required and the rest are optional. The corpus sets the two
 * payment lags, the settlement days, the index payment period and the overnight
 * lockout days in one element each, and never sets either payment calendar or
 * either payment convention.
 */
struct bma_basis_swap_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique BMA basis swap identifier.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief IBOR or overnight index the non-BMA leg pays.
     */
    std::string index;

    /**
     * @brief BMA municipal index the other leg pays.
     */
    std::string bma_index;

    /**
     * @brief Calendar the BMA leg's payments roll on, when it differs from the index's.
     */
    std::optional<std::string> bma_payment_calendar;

    /**
     * @brief Business day convention of the BMA leg's payments.
     */
    std::optional<std::string> bma_payment_convention;

    /**
     * @brief Number of business days between the BMA leg's fixing and its payment.
     */
    std::optional<int> bma_payment_lag;

    /**
     * @brief Calendar the index leg's payments roll on, when it differs from the index's.
     */
    std::optional<std::string> index_payment_calendar;

    /**
     * @brief Business day convention of the index leg's payments.
     */
    std::optional<std::string> index_payment_convention;

    /**
     * @brief Number of business days between the index leg's fixing and its payment.
     */
    std::optional<int> index_payment_lag;

    /**
     * @brief Number of business days between the index leg's fixing and its settlement.
     */
    std::optional<int> index_settlement_days;

    /**
     * @brief Payment period of the index leg, as ORE's period code.
     */
    std::optional<std::string> index_payment_period;

    /**
     * @brief Number of days before a payment date on which the overnight fixing is locked.
     */
    std::optional<int> overnight_lockout_days;

    /**
     * @brief The oresmd URI of the market data this convention needs: the index or the bma_index,
     * as a fixing URI. The single field cannot name both; which one it carries is a product
     * decision. Classification: identifier — the reference it names is fully specified.
     *
     * The column holds the address as a value, so refdata depends on the oresmd format as a
     * contract only and never on the marketdata library or its tables. Nullable, because no
     * convention is required to state its address yet.
     */
    std::optional<std::string> oresmd_uri;

    /**
     * @brief Username of the person who last modified this BMA basis swap convention.
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
    friend bool operator==(const bma_basis_swap_convention&,
                           const bma_basis_swap_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for bma_basis_swap_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bma_basis_swap_convention&) {
    return "ores.refdata.bma_basis_swap_convention";
}

}

#endif
