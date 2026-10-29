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
#ifndef ORES_ANALYTICS_API_DOMAIN_PRICING_PAIR_SETTING_HPP
#define ORES_ANALYTICS_API_DOMAIN_PRICING_PAIR_SETTING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief A setting a model and engine pair takes, with its usual value.
 *
 * The settings a pair of model and engine takes. The table states which settings a
 * pair accepts, the value ORE's examples use and whether ORE needs the setting. The
 * control the screen draws and the values it accepts belong to the value set of the
 * setting, which
 * ores_reporting_ore_parameter_setting_value_sets_tbl links by scope and name,
 * so the same rule serves every pair that takes the setting and the write path
 * that refuses a value outside it.
 */
struct pricing_pair_setting final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique code of the setting row: the product type, model, engine, scope and setting
     * name joined with a colon.
     *
     * Examples: 'BermudanSwaption:LGM:Grid:model:Calibration'.
     */
    std::string code;

    /**
     * @brief Pricing model name the setting belongs to.
     */
    std::string model;

    /**
     * @brief Numerical engine name the setting belongs to.
     */
    std::string engine;

    /**
     * @brief The pricing engine type of the pair the setting belongs to.
     *
     * Soft FK to ores_analytics_pricing_engine_types_tbl.
     */
    std::string pricing_engine_type_code;

    /**
     * @brief Where ORE writes the setting: model or engine.
     */
    std::string scope;

    /**
     * @brief The setting name as ORE writes it.
     *
     * Examples: 'Calibration', 'BootstrapTolerance'.
     */
    std::string name;

    /**
     * @brief The value ORE's examples use.
     */
    std::string usual_value;

    /**
     * @brief Whether ORE needs the setting. The guide marks a setting optional when ORE has a
     * default for it. A setting that is not optional is required, and the screens start a new
     * product with it.
     */
    bool is_required = false;

    /**
     * @brief A note on what the setting does.
     */
    std::string note;

    /**
     * @brief Username of the person who last modified this pricing pair setting.
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
    friend bool operator==(const pricing_pair_setting&, const pricing_pair_setting&) = default;
};

/**
 * @brief Dispatch-key identifier for pricing_pair_setting, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const pricing_pair_setting&) {
    return "ores.analytics.pricing_pair_setting";
}

}

#endif
