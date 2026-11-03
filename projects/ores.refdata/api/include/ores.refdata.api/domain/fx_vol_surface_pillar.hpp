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
#ifndef ORES_REFDATA_API_DOMAIN_FX_VOL_SURFACE_PILLAR_HPP
#define ORES_REFDATA_API_DOMAIN_FX_VOL_SURFACE_PILLAR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One (expiry, strike label) quoted point of an FX volatility surface blueprint.
 *
 * One quoted point a surface's calibration reads: an expiry and a strike label, ATM,
 * a risk reversal or butterfly at a delta, or a call or put at a delta, such as 25RR
 * or 10P. Unlike a curve's pillars, a surface's pillars have no order the
 * construction depends on, so a pillar is keyed by its point rather than by a
 * sequence. Its quotes come from its sources in fx_vol_surface_pillar_source, one per
 * contributor, each with the weight it carries in the calibration.
 *
 * expiry_code is a soft tenor reference, as the IR pillar's tenor codes are: the
 * expiry is resolved to a date through the tenor machinery at the application layer.
 */
struct fx_vol_surface_pillar final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this pillar row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this pillar's blueprint belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent fx_vol_surface_blueprint this pillar belongs to.
     */
    boost::uuids::uuid blueprint_id;

    /**
     * @brief The pillar's expiry as a tenor code, such as 1M or 1Y (references tenor.code).
     */
    std::string expiry_code;

    /**
     * @brief The pillar's strike label: ATM, or a delta from 1 to 99 with no leading zero followed
     * by RR, BF, C or P, such as 25RR or 10P, as the fx_option series spells it. A delta of zero
     * names no option, so 0C and 05P are refused. A label the pattern does not match gives a null
     * substring, which a check would pass, so the test is coalesced to false.
     */
    std::string strike_label;

    /**
     * @brief Username of the person who last modified this FX volatility surface pillar.
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
    friend bool operator==(const fx_vol_surface_pillar&, const fx_vol_surface_pillar&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_surface_pillar, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_surface_pillar&) {
    return "ores.refdata.fx_vol_surface_pillar";
}

}

#endif
