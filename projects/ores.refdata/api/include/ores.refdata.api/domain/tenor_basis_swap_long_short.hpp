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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_LONG_SHORT_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_SWAP_LONG_SHORT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The long and short leg form of a tenor basis swap convention.
 *
 * The long and short leg form of a tenor basis swap. ORE writes a tenor basis
 * swap as LongIndex and ShortIndex with an optional payment tenor each, plus
 * the spread flag. A row of this part holds that form. The paying and receiving
 * form lives in tenor_basis_swap_pay_receive, and a convention fills exactly
 * one of the two.
 */
struct tenor_basis_swap_long_short final {
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
     * @brief The central convention row this part belongs to.
     */
    boost::uuids::uuid convention_id;

    /**
     * @brief The party that owns the convention this part belongs to. Carried so row level security
     * can isolate the part without a join, and stamped from the session that writes the document.
     */
    boost::uuids::uuid party_id;

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
     * @brief Username of the person who last modified this tenor basis swap long short.
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
    friend bool operator==(const tenor_basis_swap_long_short&,
                           const tenor_basis_swap_long_short&) = default;
};

/**
 * @brief Dispatch-key identifier for tenor_basis_swap_long_short, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_basis_swap_long_short&) {
    return "ores.refdata.tenor_basis_swap_long_short";
}

}

#endif
