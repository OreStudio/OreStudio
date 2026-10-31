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
#ifndef ORES_REPORTING_API_DOMAIN_CUT_RESOLUTION_RULE_HPP
#define ORES_REPORTING_API_DOMAIN_CUT_RESOLUTION_RULE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief How a report resolves a cut selection to one cut.
 *
 * Reference data table of the rules a cut kind may name. A report holds a cut
 * selection, and the selection is a rule: the run executes it against the date the
 * report covers, the report's scope and the scope's rates centre, and the answer is
 * one concrete cut or a refusal.
 *
 * A rule is read from the kind at the moment of the resolution and never copied
 * onto a cut or a report, because a report resolving today must follow today's
 * rule and not the rule in force when the report was defined.
 *
 * The rules are stated on
 * [[id:F6362451-D569-4609-97A3-F1C785AAAF02][the cut design page]], section
 * *The resolution*. A kind whose occasion no rule can resolve names the_cut_named,
 * and the report reaches its cuts by pinning one rather than by selecting a kind.
 */
struct cut_resolution_rule final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique rule code.
     *
     * Examples: 'same_day_close', 'run_instant'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the rule.
     */
    std::string name;

    /**
     * @brief What the rule resolves to, in the words a person reads.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this cut resolution rule.
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
    friend bool operator==(const cut_resolution_rule&, const cut_resolution_rule&) = default;
};

/**
 * @brief Dispatch-key identifier for cut_resolution_rule, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cut_resolution_rule&) {
    return "ores.reporting.cut_resolution_rule";
}

}

#endif
