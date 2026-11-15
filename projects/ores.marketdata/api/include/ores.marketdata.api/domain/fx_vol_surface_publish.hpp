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
#ifndef ORES_MARKETDATA_API_DOMAIN_FX_VOL_SURFACE_PUBLISH_HPP
#define ORES_MARKETDATA_API_DOMAIN_FX_VOL_SURFACE_PUBLISH_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The subject, checks and publication of one FX volatility surface publish.
 *
 * The detail row of one marketdata.fx_vol_surface_publish approval request: the
 * calibrated candidate the request authorises, the checks of that candidate, and
 * the record of the act once the surface is published.
 *
 * A surface is derived data and belongs to its blueprint's party. The subject is the
 * blueprint and the as-of, which name one calibration record. The raise copies that
 * record's checks here, so the person who decides reads the fit they approve: whether
 * it converged, its largest pillar error, how many pillars are beyond the tolerance,
 * and how many arbitrage conditions it breaks. The apply publishes that calibration
 * and no later one: a calibration rerun after the raise is a new version, and the
 * apply refuses a version the request did not name.
 *
 * A blueprint's output series holds one live surface. The apply ends every live set
 * of the series in the same act, as a curve publish does, so the superseded set and
 * its successor both stay in the history. published_at and published_by are the
 * publication record, empty while the request waits. request_id is the natural
 * key, so a request authorises one publish.
 */
struct fx_vol_surface_publish final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The approval request this record is the subject of.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The party the surface belongs to: its blueprint's party. Only a person acting for it
     * raises the publish.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The fx_vol_surface_blueprint (in ores.refdata) the surface was calibrated from.
     */
    boost::uuids::uuid blueprint_id;

    /**
     * @brief The as-of of the calibration the request authorises.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The market_series the surface was written to, and whose live set this publish becomes.
     */
    boost::uuids::uuid output_series_id;

    /**
     * @brief The fx_vol_surface_calibration the request authorises.
     */
    boost::uuids::uuid calibration_id;

    /**
     * @brief The version of the calibration record the request authorises. A rerun after the raise
     * is a later version, and the apply refuses it.
     */
    int calibration_version = 0;

    /**
     * @brief Whether the calibration converged, as the raise read it.
     */
    bool converged = false;

    /**
     * @brief The largest absolute pillar error of the calibration, in volatility, as the raise read
     * it.
     */
    double max_abs_error{};

    /**
     * @brief How many pillars of the calibration are beyond the tolerance, as the raise read it.
     */
    int pillars_beyond_tolerance = 0;

    /**
     * @brief How many arbitrage conditions the calibrated surface breaks, as the raise read it.
     */
    int violation_count = 0;

    /**
     * @brief The username of the account that applied the request, or empty while it waits.
     */
    std::string published_by = "";

    /**
     * @brief When the set was published, or null while the request waits.
     */
    std::optional<std::chrono::system_clock::time_point> published_at;

    /**
     * @brief When a later set took this one's place, or null while this set is in use.
     *
     * A supersession is a publish whose subject is the successor, so the instant is the successor's
     * published instant and no person signs it: the successor's own record names the publisher.
     */
    std::optional<std::chrono::system_clock::time_point> superseded_at;

    /**
     * @brief The publish request whose apply replaced this set, or null.
     *
     * A soft reference, as the withdrawal request is: the successor is published under its own
     * request and this row links to it so a reader follows one live set and one superseded set and
     * never two live sets.
     */
    std::optional<boost::uuids::uuid> superseded_by_request_id;

    /**
     * @brief The as-of the successor was built at, or null.
     *
     * Carried beside the successor's request so the history names the set that replaced this one
     * without a second read.
     */
    std::optional<std::chrono::system_clock::time_point> superseded_as_of;

    /**
     * @brief Username of the person who last modified this FX volatility surface publish.
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
    friend bool operator==(const fx_vol_surface_publish&, const fx_vol_surface_publish&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_surface_publish, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_surface_publish&) {
    return "ores.marketdata.fx_vol_surface_publish";
}

}

#endif
