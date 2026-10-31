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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_KIND_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One kind of one volatility entry: its code, its priority and the terms it states.
 *
 * An ORE volatility entry writes either one bare kind element, or a
 * VolatilityConfig element holding up to one of each of seven kinds in an
 * xs:all. Each kind is its own complex type, and each carries a priority
 * attribute, because an entry may hold several alternatives and ORE builds the
 * first that succeeds.
 *
 * Five of the seven kinds repeat the same four terms: QuoteType,
 * VolatilityType, ExerciseType and Calendar. ApoFutureSurface states the
 * first two and no calendar, and ProxySurface states none of the four. The four
 * are columns of this row rather than of each kind's part, because the row is what
 * makes them appear once: seven kinds sharing four columns is a group, seven kinds
 * each having their own four is a union again. A kind that states none of them
 * leaves them null, and the mapper reads them only where the holder's type
 * declares them.
 *
 * This row is that kind: which kind it is, where ORE's priority puts it, and
 * the terms it states. The kind's own fields live in the kind's part, one table
 * each, as curve_volatility_strike_surface does for StrikeSurface.
 *
 * The kind is the discriminator, and it is a row rather than a column because an
 * entry holds several of them.
 */
struct curve_volatility_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The surrogate key of this kind row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The volatility entry this kind belongs to. An entry holds several kinds, so the key is
     * this column with kind_code, and the generated table adds the unique index on the pair over
     * the live rows.
     */
    boost::uuids::uuid curve_volatility_config_id;

    /**
     * @brief The ORE element this kind is: Constant, Curve, StrikeSurface, DeltaSurface,
     * MoneynessSurface, ApoFutureSurface or ProxySurface.
     */
    std::string kind_code;

    /**
     * @brief Where this kind sits in the entry's order, as ORE's priority attribute states it. ORE
     * builds the first kind that succeeds, so the order is the meaning.
     */
    std::optional<int> priority;

    /**
     * @brief The party that owns the entry this kind belongs to. Carried so row level security can
     * isolate the kind without a join.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief What the kind's quotes mean, as ORE names it.
     */
    std::optional<std::string> quote_type;

    /**
     * @brief Which volatility the kind states.
     */
    std::optional<std::string> volatility_type;

    /**
     * @brief How the exercise is typed, where the kind states one.
     */
    std::optional<std::string> exercise_type;

    /**
     * @brief The calendar the kind's observations are read on.
     */
    std::optional<std::string> calendar;

    /**
     * @brief Username of the person who last modified this curve volatility kind.
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
    friend bool operator==(const curve_volatility_kind&, const curve_volatility_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_volatility_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_volatility_kind&) {
    return "ores.refdata.curve_volatility_kind";
}

}

#endif
