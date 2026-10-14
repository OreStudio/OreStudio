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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_INSTANCE_KIND_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_INSTANCE_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The kind of a report instance.
 *
 * Reference data naming the kinds of report instance. A report instance is one
 * execution of a report definition, and its kind says what that execution does:
 *
 * - production prepares the engine's input, computes it on the grid and records the
 *   results. It is the kind of every instance the scheduler creates.
 * - package prepares the engine's input and stops. It produces the packages that the
 *   compute grid would receive for the report and computes nothing, so a developer can
 *   inspect a report's packages and run them in ORE on their own machine.
 *
 * The kind is a property of the run and not of the definition: one definition has
 * instances of both kinds. A package instance is a real instance, with its own run
 * token scope, storage keys and lifecycle, so the steps that prepare the input work for
 * it unchanged. The concurrency policy of a definition counts production instances only.
 */
struct report_instance_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code of the kind: production or package.
     */
    std::string code;

    /**
     * @brief The human-readable name of the kind.
     */
    std::string name;

    /**
     * @brief What a report instance of this kind does, in one line.
     */
    std::string description;

    /**
     * @brief The order the kind appears in a list.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this report instance kind.
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
    friend bool operator==(const report_instance_kind&, const report_instance_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for report_instance_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_instance_kind&) {
    return "ores.reporting.report_instance_kind";
}

}

#endif
