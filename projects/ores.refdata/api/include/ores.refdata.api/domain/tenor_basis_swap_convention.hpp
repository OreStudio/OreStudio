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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a tenor basis swap, where each leg pays a different tenor off its own
 * index.
 *
 * Describes how ORE builds the two legs of a tenor basis swap, where each leg
 * pays a different tenor off its own index and the two legs may differ in more
 * than the tenor. Corresponds to the <TenorBasisSwap> element in ORE
 * conventions.xml. The id field is the natural key (ORE <Id> element).
 *
 * ORE lets a tenor basis swap be written two ways, and the corpus uses both. The
 * paying and receiving form names PayIndex and ReceiveIndex with an optional
 * frequency each, and adds the averaging and sub-periods fields. The long and
 * short form names LongIndex and ShortIndex with an optional payment tenor
 * each, and adds the spread flags. The two forms do not share a field, so the
 * table carries every field of both and each row fills one form.
 */
struct tenor_basis_swap_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique tenor basis swap identifier.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Index the paying leg references, in the paying and receiving form.
     */
    std::optional<std::string> pay_index;

    /**
     * @brief Payment frequency of the paying leg, as ORE's period code.
     */
    std::optional<std::string> pay_frequency;

    /**
     * @brief Index the receiving leg references, in the paying and receiving form.
     */
    std::optional<std::string> receive_index;

    /**
     * @brief Payment frequency of the receiving leg, as ORE's period code.
     */
    std::optional<std::string> receive_frequency;

    /**
     * @brief Whether the quoted spread is added to the receiving leg rather than the paying one.
     */
    std::optional<bool> spread_on_rec;

    /**
     * @brief Whether the spread is included in the coupon that carries it.
     */
    std::optional<bool> include_spread;

    /**
     * @brief How a coupon built from sub-periods is combined, as the canonical code the mapper
     * stores.
     */
    std::optional<std::string> sub_periods_coupon_type;

    /**
     * @brief Whether the paying leg's coupons are averaged rather than compounded.
     */
    std::optional<bool> pay_is_averaged;

    /**
     * @brief Whether the receiving leg's coupons are averaged rather than compounded.
     */
    std::optional<bool> rec_is_averaged;

    /**
     * @brief Index the long leg references, in the long and short form.
     */
    std::optional<std::string> long_index;

    /**
     * @brief Payment tenor of the long leg, as ORE's period code.
     */
    std::optional<std::string> long_pay_tenor;

    /**
     * @brief Index the short leg references, in the long and short form.
     */
    std::optional<std::string> short_index;

    /**
     * @brief Payment tenor of the short leg, as ORE's period code.
     */
    std::optional<std::string> short_pay_tenor;

    /**
     * @brief Whether the quoted spread is added to the short leg rather than the long one.
     */
    std::optional<bool> spread_on_short;

    /**
     * @brief The oresmd URI of the market data this convention needs: one of pay_index,
     * receive_index, long_index or short_index, as a fixing URI. The single field cannot name all
     * four; which one it carries is a product decision. Classification: identifier — the leg it
     * names is fully specified.
     *
     * The column holds the address as a value, so refdata depends on the oresmd format as a
     * contract only and never on the marketdata library or its tables. Nullable, because no
     * convention is required to state its address yet.
     */
    std::optional<std::string> oresmd_uri;

    /**
     * @brief Username of the person who last modified this tenor basis swap convention.
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
    friend bool operator==(const tenor_basis_swap_convention&,
                           const tenor_basis_swap_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for tenor_basis_swap_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_basis_swap_convention&) {
    return "ores.refdata.tenor_basis_swap_convention";
}

}

#endif
