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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_CURVE_BASIS_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_CURVE_BASIS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The basis configuration of a commodity curve built as a basis to another.
 *
 * A commodity curve built as a basis to a base price curve, as ORE's
 * BasisConfiguration: the base price curve and its conventions, the basis
 * conventions, the day counter, the interpolation, whether the basis is added, the
 * month offset, whether the base is averaged and whether a price is a historical
 * fixing. Only the basis construction reads them, and its presence is what
 * selects that construction, so the flag the entry row held is gone.
 *
 * A commodity curve holds at most one row of this part, so the generated table
 * adds the unique index on the live rows of (tenant_id, curve_definition_id).
 */
struct commodity_curve_basis final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The curve entry this part belongs to, as the commodity curve row is keyed.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The commodity curve a basis curve is built over.
     */
    std::optional<std::string> basis_base_price_curve;

    /**
     * @brief The convention of the base curve's prices, by its id.
     */
    std::optional<std::string> basis_base_price_conventions;

    /**
     * @brief The convention of the basis quotes, by its id.
     */
    std::optional<std::string> basis_conventions;

    /**
     * @brief The day counter of the basis, as the document spells it.
     */
    std::optional<std::string> basis_day_counter;

    /**
     * @brief How the basis interpolates.
     */
    std::optional<std::string> basis_interpolation_method;

    /**
     * @brief Whether the basis is added to the base price, as the ORE boolean the document wrote.
     */
    std::optional<std::string> basis_add_basis;

    /**
     * @brief The number of months the base curve is offset by.
     */
    std::optional<int> basis_month_offset;

    /**
     * @brief Whether the base price is averaged, as the ORE boolean the document wrote.
     */
    std::optional<std::string> basis_average_base;

    /**
     * @brief Whether a base price is taken as a historical fixing, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> basis_price_as_historical_fixing;

    /**
     * @brief Username of the person who last modified this commodity curve basis.
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
    friend bool operator==(const commodity_curve_basis&, const commodity_curve_basis&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_curve_basis, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_curve_basis&) {
    return "ores.refdata.commodity_curve_basis";
}

}

#endif
