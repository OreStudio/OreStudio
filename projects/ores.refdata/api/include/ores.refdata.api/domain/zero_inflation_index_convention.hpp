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
#ifndef ORES_REFDATA_API_DOMAIN_ZERO_INFLATION_INDEX_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_ZERO_INFLATION_INDEX_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Zero-coupon inflation indices the corpus builds inflation swaps and bonds from.
 *
 * Describes a zero-coupon inflation index: the region it is published for, how
 * often it is published, how far behind its publication date it becomes
 * available, and the currency it is quoted in. Corresponds to the
 * <ZeroInflationIndex> element in ORE conventions.xml. The id field is the
 * natural key (ORE <Id> element).
 *
 * Every field is required. The corpus sets all seven in all fifty-four elements,
 * and ORE's schema holds them by value, so an element without one does not parse.
 *
 * The element carries one more field, RebasingEvents, and this table does not
 * model it: the ORE type holds it as a list of doubles, and the refdata schema has
 * no array column. No shipped file sets it. Rather than drop it silently the
 * mapper counts every element that carries one in
 * conventions_mapper::unmodelled under ZeroInflationIndex.RebasingEvents, so a
 * document that uses it is excluded from the round-trip set and named in the
 * measurement.
 */
struct zero_inflation_index_convention final {
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
     * @brief Unique zero-coupon inflation index identifier.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Region the index is published for.
     */
    std::string region_name;

    /**
     * @brief Short code for the region the index covers.
     */
    std::string region_code;

    /**
     * @brief Whether the index is revised after its first publication.
     */
    bool revised = false;

    /**
     * @brief Publication frequency of the index, as the canonical code the mapper stores.
     */
    std::string frequency;

    /**
     * @brief Lag between the index's observation date and its publication, in ORE's period form.
     */
    std::string availability_lag;

    /**
     * @brief Currency the index is quoted in.
     */
    std::string currency;

    /**
     * @brief Username of the person who last modified this zero inflation index convention.
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
    friend bool operator==(const zero_inflation_index_convention&,
                           const zero_inflation_index_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for zero_inflation_index_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const zero_inflation_index_convention&) {
    return "ores.refdata.zero_inflation_index_convention";
}

}

#endif
