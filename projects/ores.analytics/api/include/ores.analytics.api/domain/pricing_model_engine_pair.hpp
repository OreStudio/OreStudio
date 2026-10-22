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
#ifndef ORES_ANALYTICS_API_DOMAIN_PRICING_MODEL_ENGINE_PAIR_HPP
#define ORES_ANALYTICS_API_DOMAIN_PRICING_MODEL_ENGINE_PAIR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief A model and engine pair known to price one pricing engine type.
 *
 * The vocabulary the pricing engine screens read. Each row says that a model and an engine price
 * one pricing engine type, and marks the pair that ORE's examples use. The set of models and the
 * set of engines are the distinct values of the model and engine columns.
 */
struct pricing_model_engine_pair final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique code of the pair row: the product type, model and engine joined with a colon.
     *
     * Examples: 'BermudanSwaption:LGM:Grid'.
     */
    std::string code;

    /**
     * @brief Pricing model name.
     *
     * Examples: 'LGM', 'BlackBachelier', 'DiscountedCashflows'.
     */
    std::string model;

    /**
     * @brief Numerical engine name.
     *
     * Examples: 'Grid', 'AMC', 'DiscountingSwapEngine'.
     */
    std::string engine;

    /**
     * @brief The pricing engine type this pair prices.
     *
     * Soft FK to ores_analytics_pricing_engine_types_tbl.
     */
    std::string pricing_engine_type_code;

    /**
     * @brief Whether this pair is the one ORE's examples use for the product type.
     */
    bool is_usual = false;

    /**
     * @brief A note on when to choose this pair.
     */
    std::string note;

    /**
     * @brief Username of the person who last modified this pricing model engine pair.
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
    friend bool operator==(const pricing_model_engine_pair&,
                           const pricing_model_engine_pair&) = default;
};

/**
 * @brief Dispatch-key identifier for pricing_model_engine_pair, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const pricing_model_engine_pair&) {
    return "ores.analytics.pricing_model_engine_pair";
}

}

#endif
