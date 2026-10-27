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
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief A key that a product carries so the sensitivity configuration can set its own shifts.
 *
 * A sensitivity template is a key. The pricing engine configuration puts it on a product with the
 * setting SensitivityTemplate. The sensitivity configuration gives shift sizes and shift schemes to
 * the products that carry the key. The key names an asset class and a method, for example IR_FD,
 * and the row states both of them in the two forms ORE uses: the token a key carries, and the words
 * a person reads.
 *
 * The key's spelling is the tenant's. ORE resolves a template key by looking up the key the
 * sensitivity configuration states, and a configuration may state a key that ORE's own list does
 * not hold, so a tenant can add a key of its own. The table therefore holds every row to the
 * metadata it states and does not hold the key to a shape built out of it.
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
     * @brief The asset class of the products that carry the template: IR, FX, EQ, COMM, CR or ALL.
     */
    std::string asset_class;

    /**
     * @brief How the products with this template are priced: Analytical, Semianalytical, Finite
     * difference, Monte Carlo or Trade level. It is the words a person reads; method_code is the
     * token ORE puts in the key.
     */
    std::string method;

    /**
     * @brief The method as ORE spells it in a template key: Analytical, Semianalytical, FD, MC or
     * Trade. The generated key encodes it, as in IR_FD, and so does the key a tenant writes. It
     * says which shift scheme ORE applies, so a screen can read it without reading the English of
     * method.
     */
    std::string method_code;

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
