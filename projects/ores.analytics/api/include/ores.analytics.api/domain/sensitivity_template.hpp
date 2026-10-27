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
#ifndef ORES_ANALYTICS_API_DOMAIN_SENSITIVITY_TEMPLATE_HPP
#define ORES_ANALYTICS_API_DOMAIN_SENSITIVITY_TEMPLATE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief A key that a product carries so the sensitivity configuration can set its own shifts.
 *
 * A sensitivity template is a key. The pricing engine configuration puts it on a
 * product with the setting SensitivityTemplate, and the sensitivity configuration
 * gives shift sizes and shift schemes to the products that carry the key. So the
 * row states the class of product the key belongs to and the method it is priced
 * by, and both are lookups: the class is the reference data's asset class, and the
 * method is one of the five ORE defines.
 *
 * The class is the product's class and not a claim about which market keys the
 * shifts reach. A key may set shifts on keys outside its own class, which is the
 * sensitivity configuration's business and not this table's, so nothing here links
 * a template to the keys it reaches.
 *
 * The method is the token a key spells -- FD in IR_FD -- and its name is the
 * words a person reads. Four of the five methods price a class of product and
 * state it; the trade-level method is the one ORE defines without a class, so a
 * template that states it states no class, and the table checks that.
 *
 * The key's spelling is the tenant's. ORE resolves a template key by looking up the
 * key the sensitivity configuration states, and a configuration may state a key
 * that ORE's own list does not hold, so a tenant can add a key of its own. The
 * table therefore holds every row to the metadata it states and does not hold the
 * key to a shape built out of asset_class_code and method_code.
 */
struct sensitivity_template final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique template key as ORE writes it.
     *
     * Examples: 'IR_Analytical', 'EQ_MC'.
     */
    std::string code;

    /**
     * @brief The method as ORE spells it in a template key: Analytical, Semianalytical, FD, MC or
     * Trade. It says which shift scheme ORE applies, and it is the row of
     * ores_analytics_sensitivity_methods_tbl whose name a screen reads beside it.
     *
     * Soft FK to ores_analytics_sensitivity_methods_tbl.
     */
    std::string method_code;

    /**
     * @brief The class of the products that carry the template, as the reference data spells it:
     * 'interest_rates', 'fx', 'equity', 'commodity' or 'credit'. It is the row of
     * ores_analytics_sensitivity_asset_classes_tbl whose template_prefix the key spells and whose
     * name a screen reads.
     *
     * Null on a trade-level template, which is the one method ORE defines without a class.
     *
     * Soft FK to ores_refdata_asset_class_codes_tbl, through
     * ores_analytics_validate_sensitivity_asset_class_code_fn, which holds a null to be no class
     * and validates anything else against the reference data.
     */
    std::string asset_class_code;

    /**
     * @brief What the template stands for.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this sensitivity template.
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
    friend bool operator==(const sensitivity_template&, const sensitivity_template&) = default;
};

/**
 * @brief Dispatch-key identifier for sensitivity_template, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const sensitivity_template&) {
    return "ores.analytics.sensitivity_template";
}

}

#endif
