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
#ifndef ORES_MARKETDATA_API_DOMAIN_FX_VOL_SURFACE_CALIBRATION_HPP
#define ORES_MARKETDATA_API_DOMAIN_FX_VOL_SURFACE_CALIBRATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The candidate of one FX volatility surface calibration and its checks.
 *
 * A calibration fits a blueprint's smile model to the quotes its pillars read at an
 * as-of, and writes the surface as observations of the blueprint's output series.
 * Those observations are the candidate's grid. This record is what the
 * calibration found beside them: the fitted parameters, how far each pillar's
 * quote is from the surface, the arbitrage the surface admits, and the pillars
 * no contributor quoted. They belong to one calibration, not to the blueprint,
 * so they are held here, against the candidate, and the publish of the
 * candidate reads them.
 *
 * The blueprint and the as-of are the natural key. A calibration of the same
 * blueprint at the same as-of writes a new version of this record, so a rerun
 * converges on one candidate and its history keeps the earlier fits.
 *
 * The three JSON arrays are the checks, in the shapes the calibrate reply uses.
 * pillar_fits has one entry for each pillar that had a quote, naming its point,
 * the weighted quote, the fitted volatility, the error and whether the error is
 * beyond the tolerance. violations names each arbitrage condition the surface
 * breaks, by kind and expiry. missing_pillars names each pillar no contributor
 * quoted, with the series it read. The source series of each grid point, and the
 * weight each carried, are in the point's observation_lineage.
 */
struct fx_vol_surface_calibration final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this record, derived from the blueprint and the as-of so a
     * rerun names the same record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The fx_vol_surface_blueprint (in ores.refdata) the calibration followed.
     */
    boost::uuids::uuid blueprint_id;

    /**
     * @brief The instant the calibration read its quotes and forwards at, and the instant its grid
     * is dated.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The party that owns the blueprint, and so the surface.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The version of the blueprint the calibration followed.
     */
    int blueprint_version = 0;

    /**
     * @brief The market_series the grid was written to, as observations at the as-of.
     */
    boost::uuids::uuid output_series_id;

    /**
     * @brief The published fx_forward_publish the calibration read its spot and forwards from.
     */
    boost::uuids::uuid forward_publish_id;

    /**
     * @brief The model fitted, such as ssvi.
     */
    std::string smile_model;

    /**
     * @brief The fitted SSVI skew, shared by every expiry.
     */
    double rho{};

    /**
     * @brief The fitted SSVI curvature level, shared by every expiry.
     */
    double eta{};

    /**
     * @brief The fitted SSVI curvature power, shared by every expiry.
     */
    double gamma{};

    /**
     * @brief Whether the search stopped within its tolerance, with no expiry's ATM variance held at
     * its bound.
     */
    bool converged = false;

    /**
     * @brief The largest absolute error of a pillar, in volatility.
     */
    double max_abs_error{};

    /**
     * @brief How many pillars are further from the surface than the tolerance.
     */
    int pillars_beyond_tolerance = 0;

    /**
     * @brief Serialised JSON array: for each quoted pillar its point, expiry, label, weighted
     * quote, fitted volatility, error, and whether the error is beyond the tolerance.
     */
    std::string pillar_fits;

    /**
     * @brief Serialised JSON array of the arbitrage conditions the surface breaks, each a kind
     * (calendar or butterfly), an expiry and a description. Empty for a surface free of arbitrage.
     */
    std::string violations;

    /**
     * @brief Serialised JSON array of the pillars no contributor quoted at the as-of, each its
     * point and the series it read.
     */
    std::string missing_pillars;

    /**
     * @brief Username of the person who last modified this FX volatility surface calibration.
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
    friend bool operator==(const fx_vol_surface_calibration&,
                           const fx_vol_surface_calibration&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_surface_calibration, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_surface_calibration&) {
    return "ores.marketdata.fx_vol_surface_calibration";
}

}

#endif
