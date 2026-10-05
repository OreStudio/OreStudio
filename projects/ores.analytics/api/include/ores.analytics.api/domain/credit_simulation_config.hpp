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
#ifndef ORES_ANALYTICS_API_DOMAIN_CREDIT_SIMULATION_CONFIG_HPP
#define ORES_ANALYTICS_API_DOMAIN_CREDIT_SIMULATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief An ORE credit simulation configuration.
 *
 * One CreditSimulation document. The root carries the Risk block as typed
 * columns and owns the entities that migrate; the matrices are shared by name,
 * so they are their own table and an entity points at one.
 */
struct credit_simulation_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The name of the configuration, which is ours and not ORE's.
     */
    std::string name;

    /**
     * @brief The reporting configuration this content belongs to. Nullable because the content can
     * be imported before it is registered as a configuration; the mapper does not invent a header,
     * the caller supplies one.
     *
     * The foreign key into reporting's configurations is not checked here (:skip_check:): a trigger
     * runs with the inserting service's grants, and this component does not read reporting's
     * tables. Reporting's binding holds the reference; this column only names it.
     */
    boost::uuids::uuid configuration_id;

    /**
     * @brief The market risk mode the run uses.
     */
    std::string market;

    /**
     * @brief The credit risk mode the run uses.
     */
    std::string credit;

    /**
     * @brief Whether the market profit and loss is zeroed.
     */
    bool zero_market_pnl = false;

    /**
     * @brief The evaluation date or rule the run uses.
     */
    std::string evaluation;

    /**
     * @brief Whether double default is modelled.
     */
    bool double_default = false;

    /**
     * @brief The Monte Carlo seed.
     */
    int seed = 0;

    /**
     * @brief The number of Monte Carlo paths.
     */
    int paths = 0;

    /**
     * @brief The credit simulation mode.
     */
    std::string credit_mode;

    /**
     * @brief How loan exposure is computed.
     */
    std::string loan_exposure_mode;

    /**
     * @brief Username of the person who last modified this credit simulation configuration.
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
    friend bool operator==(const credit_simulation_config&,
                           const credit_simulation_config&) = default;
};

/**
 * @brief Dispatch-key identifier for credit_simulation_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const credit_simulation_config&) {
    return "ores.analytics.credit_simulation_config";
}

}

#endif
