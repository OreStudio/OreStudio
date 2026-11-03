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
#ifndef ORES_REFDATA_API_DOMAIN_FX_VOL_SURFACE_BLUEPRINT_HPP
#define ORES_REFDATA_API_DOMAIN_FX_VOL_SURFACE_BLUEPRINT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The recipe a calibration fits one FX volatility surface by -- pair, smile model, expiry
 * interpolation, output grid and output series.
 *
 * Records *how* one FX volatility surface is built, as a named, inspectable recipe, the
 * surface's counterpart of the IR curve bootstrap config. Its pillars, in
 * fx_vol_surface_pillar, name the quotes the calibration reads; the blueprint states
 * what is done with them: the smile model fitted at each expiry, how the fitted
 * expiries are joined, the grid of expiries and strikes the surface is written on, and
 * the series it is written to. See
 * [[id:53D14F31-EE32-493E-BBEB-140AA4906BE4][Constructing a volatility surface: from quotes to a
 * published set]].
 *
 * output_series_id is a soft, cross-component reference into ores.marketdata's
 * market_series: the fx_vol_surface series the calibration alone writes. As for the
 * IR config, it is chosen and not minted, and it is the sole natural key, so exactly
 * one active blueprint, tenant-wide, writes any given surface series.
 *
 * fx_option_convention_id names the pair's quoting convention in
 * fx_option_convention, a soft reference: its ATM type, delta type, butterfly style
 * and switch tenor decide what each pillar's quote means. smile_model and
 * expiry_interpolation are small, fixed vocabularies intrinsic to the record, held
 * by check constraints, as the IR config's curve_family_role is.
 *
 * The output grid is two lists, the expiries as tenor codes and the strikes in the
 * oresmd strike grammar (ATM, DEL/Spot/Put/25, an absolute level), each separated
 * by commas: the grid is a property of the recipe, read whole by the calibration and
 * never addressed one point at a time, so it has no table of its own.
 */
struct fx_vol_surface_blueprint final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this blueprint.
     */
    boost::uuids::uuid id;

    /**
     * @brief Soft reference to the fx_vol_surface market_series (in ores.marketdata) the
     * calibration writes, named by the caller from the series its desk holds. The sole natural key.
     */
    boost::uuids::uuid output_series_id;

    /**
     * @brief Owning party (legal entity) this blueprint belongs to. Set server-side from the
     * authenticated session, enforced by RLS. Not part of the natural key: the output series is
     * unique tenant-wide.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The record's display name, as a person writes it, such as EURUSD-BROKER. Typed by the
     * caller and never derived. Unique per tenant and per party among the active rows.
     */
    std::string name;

    /**
     * @brief The pair's base currency, the fx_vol_surface series' subject.
     */
    std::string base_currency_code;

    /**
     * @brief The pair's quote currency.
     */
    std::string quote_currency_code;

    /**
     * @brief Soft reference to the pair's fx_option_convention, such as EUR-USD-FXOPTION, whose
     * types and switch tenor decide what each pillar's quote means.
     */
    std::string fx_option_convention_id;

    /**
     * @brief The model fitted at each expiry: ssvi, sabr or spline. SSVI and SABR need an ATM and a
     * risk reversal and a butterfly at one delta at least; a spline needs two deltas.
     */
    std::string smile_model;

    /**
     * @brief How the fitted expiries are joined: linear_total_variance, linear in total implied
     * variance at a fixed strike measure, or flat_forward_variance, constant forward variance
     * between expiries.
     */
    std::string expiry_interpolation;

    /**
     * @brief The output grid's expiries as tenor codes, separated by commas, such as
     * 1W,1M,3M,6M,1Y,2Y.
     */
    std::string output_expiries;

    /**
     * @brief The output grid's strikes in the oresmd strike grammar, separated by commas, such as
     * DEL/Spot/Put/10,DEL/Spot/Put/25,ATM,DEL/Spot/Call/25,DEL/Spot/Call/10.
     */
    std::string output_strikes;

    /**
     * @brief Username of the person who last modified this FX volatility surface blueprint.
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
    friend bool operator==(const fx_vol_surface_blueprint&,
                           const fx_vol_surface_blueprint&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_surface_blueprint, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_surface_blueprint&) {
    return "ores.refdata.fx_vol_surface_blueprint";
}

}

#endif
