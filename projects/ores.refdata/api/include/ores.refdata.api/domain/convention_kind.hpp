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
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_KIND_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The coded catalogue of the convention kinds a central convention row may name.
 *
 * The coded catalogue of convention kinds, one row per convention an ORE
 * conventions document can carry. A central convention row names its kind with
 * this code, and the kind selects the family whose parts the row carries.
 *
 * The rows are ORE's own convention category names, upper-cased: the kind of
 * tenor_basis_swap_convention is TENOR_BASIS_SWAP. A kind that is modelled
 * beside this table in ores.refdata owns a family of parts; this table is the
 * closed set that says which kinds the central row may be.
 *
 * Batch-loaded reference data, refreshed from ORE's ore_types.xsd alongside the
 * other coded refdata lookups.
 */
struct convention_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The kind code, upper snake case. It is the value a convention row holds in kind_code.
     */
    std::string code;

    /**
     * @brief Human-readable name of the convention kind (e.g. "Tenor Basis Swap").
     */
    std::string name;

    /**
     * @brief One-line description of the convention kind.
     */
    std::string description;

    /**
     * @brief Ordinal position for dropdown/list display.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this convention kind.
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
    friend bool operator==(const convention_kind&, const convention_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for convention_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const convention_kind&) {
    return "ores.refdata.convention_kind";
}

}

#endif
