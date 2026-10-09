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
#ifndef ORES_REFDATA_API_DOMAIN_BOND_YIELD_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_BOND_YIELD_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a bond yield, the compounding and price type it is quoted on.
 *
 * Describes how ORE builds a bond yield: the compounding it is quoted on, the
 * frequency it pays at, whether the price is clean or dirty, and the tolerances
 * its solver runs to. Corresponds to the <BondYield> element in ORE conventions.xml.
 * The id field is the natural key (ORE <Id> element).
 *
 * One field is required and six are optional. One file carries five elements and
 * sets all six optional fields. The accuracy and the guess are floats in ORE and
 * travel through a double column, because the refdata schema has no float type;
 * a float widens to a double and narrows back with no loss.
 */
struct bond_yield_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique bond yield identifier.
     */
    std::string id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Compounding the yield is quoted on, as ORE spells it.
     */
    std::string compounding;

    /**
     * @brief Frequency the bond pays at, as the canonical code the mapper stores.
     */
    std::optional<std::string> frequency;

    /**
     * @brief Whether the quoted price is clean or dirty, as ORE spells it.
     */
    std::optional<std::string> price_type;

    /**
     * @brief Tolerance the yield solver converges to.
     */
    std::optional<double> accuracy;

    /**
     * @brief Maximum number of solver iterations.
     */
    std::optional<int> max_evaluations;

    /**
     * @brief Starting guess the yield solver takes.
     */
    std::optional<double> guess;

    /**
     * @brief The oresmd URI of the market data this convention needs. The convention sets only the
     * solver parameters (compounding, price type, accuracy, guess), so it names no series.
     * Classification: requirement — the address states the bond price series a yield is solved
     * from, with the bond and the point left open. Uncertain: the convention names no bond, so the
     * address is a curve-level requirement at best.
     *
     * The column holds the address as a value, so refdata depends on the oresmd format as a
     * contract only and never on the marketdata library or its tables. Nullable, because no
     * convention is required to state its address yet.
     */
    std::optional<std::string> oresmd_uri;

    /**
     * @brief Username of the person who last modified this bond yield convention.
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
    friend bool operator==(const bond_yield_convention&, const bond_yield_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for bond_yield_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_yield_convention&) {
    return "ores.refdata.bond_yield_convention";
}

}

#endif
