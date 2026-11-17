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
 * @brief One field of one series family's identity, and whether the family requires it or forbids
 * it.
 *
 * The shape of a market series identity, one row per field per family.
 *
 * A family is the pair of codes the taxonomy already carries -- the asset class
 * and the series subclass -- narrowed, for an index series, by the index family.
 * index_fixing is not one shape but several: an IBOR fixing states a currency
 * and an index name, a swap fixing a currency and a tenor, an FX fixing a currency
 * and the currency it is quoted in. A rule keyed on the subclass alone would be
 * wrong about all but one of them, so the key carries the index family too.
 *
 * A row says what the family makes of one field: required means a row of this
 * family that leaves it empty is not an identity, forbidden means the field says
 * nothing about this family and a row that fills it states something untrue. A
 * field a family does not mention is neither: the family does not use it, and the
 * writer leaves it as the URI parse left it.
 *
 * The table is read by the writer that projects an identity row from an oresmd
 * URI, which refuses a row that misses a required field or carries a forbidden
 * one, naming the family and the field. It sits beside
 * series_classification_rules because the two are the same kind of statement --
 * what a series of this identity is -- and because a family the taxonomy gains
 * gains its shape in the same place rather than in a second mechanism.
 *
 * A family with no row here is not enforced: the writer projects its series
 * unguarded, and the backfill counts them as unstated. The catalogue grows family by
 * family, and refusing every family nobody has stated yet would stop an ingest for a
 * reason that is the catalogue's.
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
     * @brief The family's asset class, as the oresmd authority spells it and as the identity row
     * carries it: ir, fx, credit. This is the codec's spelling and not
     * ores.refdata.asset_class_code's -- refdata calls the same class interest_rates -- so the
     * column is checked against the oresmd schema rather than against that catalogue, and a family
     * keyed by one spelling and looked up by the other would never match.
     *
     * The same subclass of two classes is two families, so this column is half of the key rather
     * than a description of it.
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
     * by its tenor, and a row that stated one of them for both would be wrong about one of them. So
     * the key carries the family.
     */
    std::string index_family;

    /**
     * @brief The identity column the row speaks about, named as market_series_identity holds it:
     * ccy, unit_ccy, index, instrument_type.
     *
     * A name that is no column of that table is a statement nobody can act on, so
     * build/scripts/check_marketdata_identity_columns.py checks every seeded row against the
     * table's columns at build time.
     */
    std::string field;

    /**
     * @brief Whether the family requires the field or forbids it: required or forbidden.
     */
    std::string requirement;

    /**
     * @brief Why the family makes that of the field, in one sentence.
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
