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
#ifndef ORES_SYNTHETIC_API_DOMAIN_CALIBRATION_POLICY_HPP
#define ORES_SYNTHETIC_API_DOMAIN_CALIBRATION_POLICY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief The calibration rule for one asset class: window, inverse and fallback.
 *
 * A policy is the class-wide answer to how a feed is calibrated: the volatility
 * window in days, whether a pair may be read the other way round as 1/x, and
 * what to do when no candidate has a usable fixing. The provider order lives in
 * calibration_provider_order rows.
 *
 * A policy row is optional. When no row exists for a class, the defaults apply:
 * providers in catalogue display order, a 90-day window, an inverse allowed and
 * the keep_fixed fallback. The system tenant owns the rows and every tenant
 * reads them.
 */
struct calibration_policy final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying the policy.
     */
    boost::uuids::uuid id;

    /**
     * @brief The asset class the policy governs (soft FK to asset_class_code.code): fx today, with
     * the other classes to follow. One policy per class.
     */
    std::string asset_class_code;

    /**
     * @brief The volatility window, in days, over which the fixing returns are measured.
     */
    int window_days = 90;

    /**
     * @brief Whether a pair may be read from an item that quotes it the other way round, as 1/x.
     */
    bool allow_inverse = true;

    /**
     * @brief What a calibration does when no candidate has a usable fixing (soft FK to
     * calibration_fallback.code).
     */
    std::string fallback_code = "keep_fixed";

    /**
     * @brief Username of the person who last modified this calibration policy.
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
    friend bool operator==(const calibration_policy&, const calibration_policy&) = default;
};

/**
 * @brief Dispatch-key identifier for calibration_policy, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calibration_policy&) {
    return "ores.synthetic.calibration_policy";
}

}

#endif
