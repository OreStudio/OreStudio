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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_IDENTITY_SHAPE_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_IDENTITY_SHAPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The identity shape of one series family, named by a code and described in words.
 *
 * The shape of a market series identity, one row per family.
 *
 * A family is the pair of codes the taxonomy already carries -- the asset class
 * and the series subclass -- narrowed, for an index series, by the index family.
 * index_fixing is not one shape but several: an IBOR fixing states a currency
 * and an index name, a swap fixing a currency and a tenor, an FX fixing a currency
 * and the currency it is quoted in. A shape keyed on the subclass alone would be
 * wrong about all but one of them, so the family carries the index family too.
 *
 * A shape is named by a code, such as ir.index_fixing.ibor, so a person and a
 * screen can refer to it by one key, and it says in words what the family is for.
 * What the family makes of each field is stated by its entries,
 * series_identity_shape_entries: one row per field, required or forbidden.
 *
 * The writer that projects an identity row from an oresmd URI reads the shapes
 * and their entries, and refuses a row that misses a required field or carries a
 * forbidden one, naming the family and the field. A family with no shape is not
 * enforced: the writer projects its series unguarded, and the backfill counts them
 * as unstated. The catalogue grows family by family, and refusing every family
 * nobody has stated yet would stop an ingest for a reason that is the catalogue's.
 */
struct series_identity_shape final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The shape's code: the asset class, the series subclass and, for an index series, the
     * index family, joined by dots, such as ir.index_fixing.ibor or fx.spot. The entries name their
     * shape by it. A check holds the code to the family's parts, so a code never names one family
     * while the row states another.
     *
     * A generated shape takes its index family from the generator's counter, and its code from the
     * same parts, so each one holds the check and a family of its own.
     */
    std::string code;

    /**
     * @brief The family's asset class, as the oresmd authority spells it and as the identity row
     * carries it: ir, fx, credit. This is the codec's spelling and not
     * ores.refdata.asset_class_code's -- refdata calls the same class interest_rates -- so the
     * column is checked against the oresmd schema rather than against that catalogue, and a family
     * keyed by one spelling and looked up by the other would never match.
     */
    std::string asset_class;

    /**
     * @brief The family's series subclass, as ores.refdata.series_subclass_code spells it:
     * index_fixing, spot, volatility.
     */
    std::string series_subclass;

    /**
     * @brief The index family the shape is for, as the identity's index column spells it: ibor,
     * swap, fx. A datum family has no index and holds the empty string.
     *
     * ir/index_fixing is two shapes, not one: an IBOR fixing is named by its index, a swap fixing
     * by its tenor, and a shape that stated one of them for both would be wrong about one of them.
     * So the family carries the index family too.
     */
    std::string index_family;

    /**
     * @brief What the family is and what a series of it is for, in words a person reads on the
     * shapes screen.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this series identity shape.
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
    friend bool operator==(const series_identity_shape&, const series_identity_shape&) = default;
};

/**
 * @brief Dispatch-key identifier for series_identity_shape, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_identity_shape&) {
    return "ores.marketdata.series_identity_shape";
}

}

#endif
