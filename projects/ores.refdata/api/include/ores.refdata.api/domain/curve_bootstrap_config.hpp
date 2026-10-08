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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_BOOTSTRAP_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_BOOTSTRAP_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The bootstrap settings of one ORE curve entry.
 *
 * The BootstrapConfig of one curve entry. Five section types carry the same
 * element, so it is one table keyed to the entry rather than columns on each
 * section's row. A row exists exactly when the entry writes the element: fourteen
 * corpus yield curves write it empty, and the export writes them back empty.
 */
struct curve_bootstrap_config final {
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
     * @brief The default curve configuration whose list holds the row, or nil when it belongs to
     * the curve entry or one of its segments.
     */
    boost::uuids::uuid default_curve_configuration_id;

    /**
     * @brief The accuracy each instrument is bootstrapped to.
     */
    std::optional<double> accuracy;

    /**
     * @brief The accuracy of a global bootstrap.
     */
    std::optional<double> global_accuracy;

    /**
     * @brief Whether a failed bootstrap keeps its best result instead of failing.
     */
    std::optional<bool> dont_throw;

    /**
     * @brief How many times the bootstrap is tried.
     */
    std::optional<int> max_attempts;

    /**
     * @brief The largest factor the bootstrap widens its search bounds by.
     */
    std::optional<double> max_factor;

    /**
     * @brief The smallest factor the bootstrap widens its search bounds by.
     */
    std::optional<double> min_factor;

    /**
     * @brief How many steps a failed bootstrap keeps when it does not fail.
     */
    std::optional<int> dont_throw_steps;

    /**
     * @brief Whether the curve is bootstrapped globally rather than pillar by pillar.
     */
    std::optional<bool> global;

    /**
     * @brief The smoothness weight of a global bootstrap.
     */
    std::optional<double> smoothness_lambda;

    /**
     * @brief Username of the person who last modified this curve bootstrap config.
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
    friend bool operator==(const curve_bootstrap_config&, const curve_bootstrap_config&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_bootstrap_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_bootstrap_config&) {
    return "ores.refdata.curve_bootstrap_config";
}

}

#endif
