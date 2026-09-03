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
#ifndef ORES_SYNTHETIC_API_DOMAIN_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_HPP
#define ORES_SYNTHETIC_API_DOMAIN_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief Reference data: named parameters each yield curve process type accepts.
 *
 * Reference data table describing, per yield_curve_process_type, the
 * named parameters that process accepts and their validation bounds. This
 * is the "vocabulary" half of the row-based parameter architecture: an
 * ir_curve_generation_config stores its process parameters as
 * ir_curve_generation_config_process_parameter_value rows, one per
 * definition, and the mapping layer materialises those rows into the
 * strongly-typed process-parameter structs of ores.analytics.quant
 * (two_factor_gaussian_params, vasicek_params, ...). The
 * (process_type_code, parameter_name) pair uniquely identifies a
 * parameter; four fields drive the Qt parameter table: display_name
 * (the English name), symbol (the Greek letter, where one is
 * conventional), short_label (the layperson name shown in Simple
 * mode) and description (the rich tooltip text). min_value/
 * max_value (NULL = unbounded) plus default_value drive the
 * dialog's spin-box ranges and pre-fill.
 *
 * Why this exists as a table rather than hardcoded structs: it makes the
 * parameter vocabulary queryable and extensible -- adding a new model or
 * parameter is a seed-data change, not a schema or code change, and the
 * same vocabulary drives the DB (validation of value rows), the mapping
 * layer (expected parameter names), and the UI (rows to display). Managed
 * by the system tenant as read-only reference data; tenant users never
 * edit definitions, only the values of their own configs.
 */
struct yield_curve_process_parameter_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this parameter definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief The yield_curve_process_type.code this parameter belongs to (soft FK to
     * ores_synthetic_yield_curve_process_types_tbl, validated via the system-tenant validation
     * function -- the same mechanism ir_curve_generation_config.process_type uses). Examples:
     * 'VASICEK', 'COX_INGERSOLL_ROSS', 'HULL_WHITE', 'TWO_FACTOR_GAUSSIAN'.
     */
    std::string process_type_code;

    /**
     * @brief Machine-readable name of the parameter, as used by the mapping layer and the value
     * rows, e.g. kappa, theta, sigma, initial_rate for the one-factor models; kappa_x, kappa_y,
     * sigma_x, sigma_y, rho, theta, initial_rate for the two-factor Gaussian. Unique within a
     * process type.
     */
    std::string parameter_name;

    /**
     * @brief English name of the parameter for display, e.g. "Mean reversion speed" for kappa.
     * Shown in the Advanced parameter table as the parenthesised meaning next to the Greek symbol.
     */
    std::string display_name;

    /**
     * @brief Greek letter conventionally used for the parameter, where one exists, e.g. κ for
     * kappa, ρ for rho; NULL when no conventional symbol applies (e.g. initial_rate). Shown as the
     * label of the Advanced parameter table row, with the English name in brackets.
     */
    std::optional<std::string> symbol;

    /**
     * @brief Short layperson name for the parameter, e.g. "Reversion speed" for kappa. Shown as the
     * row label in the Simple parameter table -- write it for a non-specialist reader, not a quant.
     */
    std::string short_label;

    /**
     * @brief Rich, user-facing description of what the parameter means and its domain constraints
     * (e.g. "Correlation between the two factor Brownian motions; must lie in [-1, 1]"). This is
     * the text the Qt parameter table shows next to each value field -- write it for a
     * non-specialist reader, not a quant.
     */
    std::string description;

    /**
     * @brief Data type of the parameter's value. "double" today (all current process parameters are
     * scalars); future models may add e.g. "text" or "vector". Not enforced by a check for now --
     * the mapping layer validates against its own per-process expectations.
     */
    std::string data_type = "double";

    /**
     * @brief Value a new config is pre-filled with when this parameter's row is added to the Qt
     * table. Not a fallback for missing rows -- the mapping layer throws on a missing parameter
     * rather than silently using a default; this is purely a UI convenience.
     */
    double default_value = 0.0;

    /**
     * @brief Lower bound of the parameter's valid range (inclusive), enforced by the Qt spin box
     * and the mapping layer; NULL means unbounded. E.g. kappa_x min 0 (a negative mean-reversion
     * speed is ill-posed for the two-factor Gaussian), rho min -1.
     */
    std::optional<double> min_value;

    /**
     * @brief Upper bound of the parameter's valid range (inclusive), enforced by the Qt spin box
     * and the mapping layer; NULL means unbounded. E.g. rho max 1.
     */
    std::optional<double> max_value;

    /**
     * @brief Order in which this parameter's row appears in the Qt parameter table, within its
     * process type. The process's own natural order (e.g. kappa_x before rho) should match the
     * strong-typed struct's field order.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this yield curve process parameter
     * definition.
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
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for yield_curve_process_parameter_definition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const yield_curve_process_parameter_definition&) {
    return "ores.synthetic.yield_curve_process_parameter_definition";
}

}

#endif
