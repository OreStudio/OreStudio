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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_BASIS_TWO_SWAP_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_BASIS_TWO_SWAP_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a two-tenor basis swap, whose two legs pay different frequencies off
 * different indices.
 *
 * Describes how ORE builds both legs of a two-tenor basis swap, where a long leg
 * and a short leg pay different frequencies off different indices, and which of
 * the two the quoted spread is measured from. Corresponds to the
 * <TenorBasisTwoSwap> element in ORE conventions.xml. The id field is the natural
 * key (ORE <Id> element).
 */
struct tenor_basis_two_swap_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Unique two-tenor basis swap identifier.
     */
    std::string id;

    /**
     * @brief Calendar both legs' schedules roll on (e.g. 'TARGET').
     */
    std::string calendar;

    /**
     * @brief Payment frequency of the long leg, as the canonical code the mapper stores.
     */
    std::string long_fixed_frequency;

    /**
     * @brief Business day convention of the long leg.
     */
    std::string long_fixed_convention;

    /**
     * @brief Day count fraction of the long leg.
     */
    std::string long_fixed_day_count_fraction;

    /**
     * @brief Index the long leg pays.
     */
    std::string long_index;

    /**
     * @brief Payment frequency of the short leg.
     */
    std::string short_fixed_frequency;

    /**
     * @brief Business day convention of the short leg.
     */
    std::string short_fixed_convention;

    /**
     * @brief Day count fraction of the short leg.
     */
    std::string short_fixed_day_count_fraction;

    /**
     * @brief Index the short leg pays.
     */
    std::string short_index;

    /**
     * @brief Whether the quoted spread is the long leg's rate minus the short leg's rather than the
     * other way round.
     */
    std::optional<bool> long_minus_short;

    /**
     * @brief Username of the person who last modified this two-tenor basis swap convention.
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
    friend bool operator==(const tenor_basis_two_swap_convention&,
                           const tenor_basis_two_swap_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for tenor_basis_two_swap_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_basis_two_swap_convention&) {
    return "ores.refdata.tenor_basis_two_swap_convention";
}

}

#endif
