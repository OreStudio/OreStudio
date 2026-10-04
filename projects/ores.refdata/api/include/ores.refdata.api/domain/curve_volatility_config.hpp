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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_VOLATILITY_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One surface a volatility entry may be built as, with its priority.
 *
 * One way a volatility entry's surface may be built. CDS, equity and commodity
 * volatilities each name one or more volatility configurations, each with an
 * optional priority; ORE builds the first that succeeds. kind names the
 * element the document wrote, and the columns that kind uses are set. An entry
 * writes a kind either directly or inside a VolatilityConfig element, which
 * holds at most one of each kind; is_wrapped says which.
 *
 * The kinds the corpus writes are modelled: Constant, Curve, StrikeSurface,
 * DeltaSurface and ProxySurface. A Curve kind's quotes are rows of
 * curve_quote on the entry, in the list Curve or VolatilityConfig/Curve.
 * MoneynessSurface, ApoFutureSurface and a parametric smile inside a surface
 * appear in no corpus document, and the mapper refuses them.
 */
struct curve_volatility_config final {
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
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The element the document wrote, such as StrikeSurface.
     */
    std::string kind;

    /**
     * @brief Whether the configuration is written inside a VolatilityConfig element.
     */
    bool is_wrapped = false;

    /**
     * @brief The configuration's priority attribute, when it has one.
     */
    std::optional<int> priority;

    /**
     * @brief What the quotes are, such as ImpliedVolatility or Premium.
     */
    std::optional<std::string> quote_type;

    /**
     * @brief Whether the volatility is normal, lognormal or shifted lognormal.
     */
    std::optional<std::string> volatility_type;

    /**
     * @brief The exercise type of quoted premiums.
     */
    std::optional<std::string> exercise_type;

    /**
     * @brief The strikes of a strike surface, as ORE's comma separated text or *.
     */
    std::optional<std::string> strikes;

    /**
     * @brief The expiries of a surface, as ORE's comma separated text or *.
     */
    std::optional<std::string> expiries;

    /**
     * @brief How the surface interpolates in time.
     */
    std::optional<std::string> time_interpolation;

    /**
     * @brief How the surface interpolates in strike.
     */
    std::optional<std::string> strike_interpolation;

    /**
     * @brief How the configuration extrapolates: for a surface, whether it extrapolates as the ORE
     * boolean the document wrote; for a curve, the extrapolation type.
     */
    std::optional<std::string> extrapolation;

    /**
     * @brief How the surface extrapolates in time.
     */
    std::optional<std::string> time_extrapolation;

    /**
     * @brief Whether time extrapolation is in variance, as the ORE boolean the document wrote.
     */
    std::optional<std::string> time_extrapolation_variance;

    /**
     * @brief How the surface extrapolates in strike.
     */
    std::optional<std::string> strike_extrapolation;

    /**
     * @brief The configuration's calendar, as the document spells it.
     */
    std::optional<std::string> calendar;

    /**
     * @brief The quote of a constant volatility.
     */
    std::optional<std::string> quote;

    /**
     * @brief How a volatility curve interpolates.
     */
    std::optional<std::string> interpolation;

    /**
     * @brief Whether a volatility curve enforces monotone variance.
     */
    std::optional<bool> enforce_monotone_variance;

    /**
     * @brief The delta type of a delta surface.
     */
    std::optional<std::string> delta_type;

    /**
     * @brief The ATM type of a delta surface.
     */
    std::optional<std::string> atm_type;

    /**
     * @brief The delta type of a delta surface's ATM strike.
     */
    std::optional<std::string> atm_delta_type;

    /**
     * @brief The put deltas of a delta surface, as ORE's comma separated text.
     */
    std::optional<std::string> put_deltas;

    /**
     * @brief The call deltas of a delta surface, as ORE's comma separated text.
     */
    std::optional<std::string> call_deltas;

    /**
     * @brief Whether a delta surface corrects for the future price, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> future_price_correction;

    /**
     * @brief The volatility a proxy surface is built from.
     */
    std::optional<std::string> proxy_volatility_curve;

    /**
     * @brief The FX volatility a proxy surface converts with.
     */
    std::optional<std::string> fx_volatility_curve;

    /**
     * @brief The correlation a proxy surface converts with.
     */
    std::optional<std::string> correlation_curve;

    /**
     * @brief The CDS volatility a proxy surface is built from.
     */
    std::optional<std::string> cds_volatility_curve;

    /**
     * @brief The configuration's place among the entry's configurations, which the export restores.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this curve volatility config.
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
    friend bool operator==(const curve_volatility_config&,
                           const curve_volatility_config&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_volatility_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_volatility_config&) {
    return "ores.refdata.curve_volatility_config";
}

}

#endif
