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
#ifndef ORES_MARKETDATA_API_DOMAIN_SERIES_IDENTITY_SHAPE_ENTRY_HPP
#define ORES_MARKETDATA_API_DOMAIN_SERIES_IDENTITY_SHAPE_ENTRY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One field of an identity shape, and whether the shape requires it or forbids it.
 *
 * An entry of an identity shape, one row per field the shape speaks about.
 *
 * The shape it belongs to is named by its code, and the shape holds the family:
 * the asset class, the series subclass and the index family. The entry says what
 * the family makes of one field: required means a row of this family that
 * leaves it empty is not an identity, forbidden means the field says nothing
 * about this family and a row that fills it states something untrue. A field a
 * shape does not mention is neither: the family does not use it, and the writer
 * leaves it as the URI parse left it.
 *
 * The writer that projects an identity row from an oresmd URI reads the shapes
 * and their entries together, and refuses a row that misses a required field or
 * carries a forbidden one, naming the family and the field.
 */
struct series_identity_shape_entry final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID that names this entry. An entry is one field of one shape, so the pair of
     * shape_code and field is unique among the current rows, by the declared index below. A
     * surrogate key names the row by one value, so its history reads as any other record's.
     */
    boost::uuids::uuid id;

    /**
     * @brief The code of the identity shape the entry belongs to, such as ir.index_fixing.ibor.
     */
    std::string shape_code;

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
     * @brief Whether the shape requires the field or forbids it: required or forbidden.
     */
    std::string requirement;

    /**
     * @brief Why the shape makes that of the field, in one sentence.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this series identity shape entry.
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
    friend bool operator==(const series_identity_shape_entry&,
                           const series_identity_shape_entry&) = default;
};

/**
 * @brief Dispatch-key identifier for series_identity_shape_entry, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_identity_shape_entry&) {
    return "ores.marketdata.series_identity_shape_entry";
}

}

#endif
