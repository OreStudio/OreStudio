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
#ifndef ORES_IAM_API_DOMAIN_SEED_PROFILE_STEP_HPP
#define ORES_IAM_API_DOMAIN_SEED_PROFILE_STEP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief One step kind a seed profile orders.
 *
 * A seed profile's steps, in the order provisioning runs them. The row names
 * a step kind from the catalogue in code and supplies the arguments that
 * kind consumes, so a profile states the same step with different bundles
 * without a code change.
 *
 * The catalogue is fixed: publish_bundle, import_lei_hierarchy,
 * provision_party, load_staff, attach_photos and
 * start_market_feeds. A step kind that is not in the catalogue is a code
 * change, because one new kind means one new step handler.
 *
 * The pair (seed_profile_id, step_kind) is the identity: a profile runs a
 * kind once. Steps are idempotent, and a failed step stops the provisioned
 * instance at that step.
 */
struct seed_profile_step final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate identifier for the step row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Profile this step belongs to.
     */
    boost::uuids::uuid seed_profile_id;

    /**
     * @brief Step kind from the fixed catalogue, for example publish_bundle or provision_party.
     */
    std::string step_kind;

    /**
     * @brief Arguments the step kind consumes, as a JSON object. The step kind fixes the shape of
     * the arguments in code and the profile supplies the values; an empty object states that the
     * kind takes none. A profile that orders publish_bundle names its bundles here, because which
     * bundles a profile publishes is profile data and not a step-kind constant.
     */
    std::string arguments_json;

    /**
     * @brief Order provisioning runs this step in. Lower numbers run first.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this seed profile step.
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
    friend bool operator==(const seed_profile_step&, const seed_profile_step&) = default;
};

/**
 * @brief Dispatch-key identifier for seed_profile_step, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const seed_profile_step&) {
    return "ores.iam.seed_profile_step";
}

}

#endif
