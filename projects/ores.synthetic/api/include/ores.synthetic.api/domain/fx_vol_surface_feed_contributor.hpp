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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_VOL_SURFACE_FEED_CONTRIBUTOR_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_VOL_SURFACE_FEED_CONTRIBUTOR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A contributor that quotes a synthetic FX volatility surface.
 *
 * Says that a contributor quotes a feed's surface. A feed with several
 * contributors gives a tenant several broker quotes of one market; the clean
 * contributor among them gives the truth.
 */
struct fx_vol_surface_feed_contributor final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party: the system party, which runs the installation's feeds.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The fx_vol_surface_feed quoted.
     */
    boost::uuids::uuid feed_id;

    /**
     * @brief The fx_vol_contributor that quotes it.
     */
    boost::uuids::uuid contributor_id;

    /**
     * @brief Username of the person who last modified this FX volatility surface feed contributor.
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
    friend bool operator==(const fx_vol_surface_feed_contributor&,
                           const fx_vol_surface_feed_contributor&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_surface_feed_contributor, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_surface_feed_contributor&) {
    return "ores.synthetic.fx_vol_surface_feed_contributor";
}

}

#endif
