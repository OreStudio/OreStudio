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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_VOL_CONTRIBUTOR_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_VOL_CONTRIBUTOR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A synthetic volatility contributor: the provider it publishes as and how its quotes differ
 * from the clean ones.
 *
 * A synthetic contributor, a provider of the simulator that quotes FX volatility
 * as a broker does. Its noise profile is its own, whatever pair it quotes, so it
 * is held once here and fx_vol_surface_feed_contributor says which pairs it
 * quotes. A profile of zeros is the clean contributor, whose quotes are the truth.
 *
 * The noise of a point is noise, widened on the wings by
 * 1 + wing_widening * (0.5 - delta) and for long expiries by
 * 1 + expiry_widening * (years - 1) past one year.
 */
struct fx_vol_contributor final {
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
     * @brief The provider the contributor publishes as.
     */
    std::string provider_code;

    /**
     * @brief The standard deviation of the noise at an ATM point of one year, in volatility.
     */
    double noise{};

    /**
     * @brief How much larger the noise is on a wing.
     */
    double wing_widening{};

    /**
     * @brief How much larger the noise is for each year past one.
     */
    double expiry_widening{};

    /**
     * @brief The step the contributor rounds its quotes to; zero for none.
     */
    double quote_step{};

    /**
     * @brief The chance a point is not published at a step.
     */
    double missing_probability{};

    /**
     * @brief The chance a point repeats the contributor's last value for it.
     */
    double stale_probability{};

    /**
     * @brief The chance a point is off by the outlier size, in either direction.
     */
    double outlier_probability{};

    /**
     * @brief How far an outlier is off, in volatility.
     */
    double outlier_size{};

    /**
     * @brief A persistent offset of every point, in volatility.
     */
    double bias{};

    /**
     * @brief Username of the person who last modified this FX volatility contributor.
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
    friend bool operator==(const fx_vol_contributor&, const fx_vol_contributor&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_vol_contributor, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_vol_contributor&) {
    return "ores.synthetic.fx_vol_contributor";
}

}

#endif
