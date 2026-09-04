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
#ifndef ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_GENERATION_CONFIG_PROCESS_PARAMETER_VALUE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_GENERATION_CONFIG_PROCESS_PARAMETER_VALUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief One named process-parameter value of an IR curve generation config.
 *
 * One named process-parameter value of an ir_curve_generation_config,
 * referencing its yield_curve_process_parameter_definition. This is the
 * "value" half of the row-based parameter architecture: an IR curve
 * generation config stores its process parameters as one row per
 * parameter (kappa0.1, rho-0.5, ...), the parameter
 * definition supplies the vocabulary (name, description, bounds), and
 * the mapping layer materialises these rows into the strongly-typed
 * process-parameter structs of ores.analytics.quant
 * (two_factor_gaussian_params, vasicek_params, ...).
 *
 * The (config_id, parameter_definition_id) pair is unique: each
 * parameter appears at most once per config. A hand-written DB trigger
 * (not the codegen single-argument Validations mechanism, which cannot
 * express a two-column lookup) enforces that the definition's
 * process_type_code matches the config's process_type -- a config
 * driven by TWO_FACTOR_GAUSSIAN may only carry values whose definitions
 * belong to TWO_FACTOR_GAUSSIAN. Both soft FKs are additionally
 * validated at insert time by the generated validation functions. No
 * standalone Qt layer: value rows are edited from inside the IR curve
 * generation config detail dialog, which reads definitions and value
 * rows via the normal codegen repositories.
 */
struct ir_curve_generation_config_process_parameter_value final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this parameter value row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Parent ir_curve_generation_config this parameter value belongs to (soft FK to
     * ores_synthetic_ir_curve_generation_configs_tbl, validated via the generated validation
     * function).
     */
    boost::uuids::uuid config_id;

    /**
     * @brief The yield_curve_process_parameter_definition this value instantiates (soft FK to
     * ores_synthetic_process_parameter_definitions_tbl, validated via the generated validation
     * function). The definition supplies the parameter's name, description and validation bounds;
     * only the value itself lives here.
     */
    boost::uuids::uuid parameter_definition_id;

    /**
     * @brief The value of this parameter for this config, in the definition's data_type ("double"
     * for all current process parameters). Must lie within the definition's min_value/max_value
     * range (NULL = unbounded); the Qt dialog's spin boxes enforce this on entry and the mapping
     * layer re-checks it before constructing the process.
     */
    double parameter_value = 0.0;

    /**
     * @brief Username of the person who last modified this IR curve generation config process
     * parameter value.
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
 * @brief Dispatch-key identifier for ir_curve_generation_config_process_parameter_value, e.g. for
 * the generic history-diff request and action registries. Single source of truth: every call site
 * spells entity_type_of(value) regardless of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const ir_curve_generation_config_process_parameter_value&) {
    return "ores.synthetic.ir_curve_generation_config_process_parameter_value";
}

}

#endif
