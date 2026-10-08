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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_ANALYTIC_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_ANALYTIC_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief An analytic a report's run asks for.
 *
 * One row per Analytic element in a report's ORE run document: which analytic to
 * run, in what order, and whether it is on. The corpus runs eighteen hundred and
 * seventy-four analytics across its four hundred and sixteen run documents, in
 * thirty-five distinct types, and every one of them carries an active parameter.
 *
 * This is the row that replaces the analytic switches on risk_report_config.
 * Those were twelve *_enabled booleans, which could say "run NPV" and nothing
 * else; a run can now name any of the thirty-five types, in order, and repeat one.
 * The type-specific parameters live in a table per type, named by the type's
 * parameter_entity, rather than in columns here.
 *
 * Each row belongs to exactly one report_definition, which is this run's
 * document, and the definition has many analytics.
 */
struct report_analytic final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this analytic.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The report definition whose run document this analytic belongs to.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The analytic to run, as the value ORE writes in its type attribute.
     */
    std::string analytic_type_code;

    /**
     * @brief The position the analytic holds in the document's list, counting from one. ORE runs
     * the analytics in the order they appear.
     */
    int display_order = 0;

    /**
     * @brief Whether the analytic is on, as ORE spells it: Y or N. ORE writes the flag as a
     * parameter of every analytic, so the row carries it rather than inferring it.
     */
    std::string active;

    /**
     * @brief Username of the person who last modified this report analytic.
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
    friend bool operator==(const report_analytic&, const report_analytic&) = default;
};

/**
 * @brief Dispatch-key identifier for report_analytic, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_analytic&) {
    return "ores.reporting.report_analytic";
}

}

#endif
