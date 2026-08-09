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
#ifndef ORES_SYNTHETIC_API_DOMAIN_YIELD_CURVE_PROCESS_PARAMETER_MAPPING_HPP
#define ORES_SYNTHETIC_API_DOMAIN_YIELD_CURVE_PROCESS_PARAMETER_MAPPING_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/export.hpp"
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace ores::synthetic::domain {

/**
 * @brief Materialise a strongly-typed yield-curve process from the
 * row-based (EAV) parameter store.
 *
 * The row-based parameter architecture stores process parameters as
 * {parameter_definition_id, value} rows (per config) plus a reference
 * catalogue of parameter definitions (name, description, min/max) per
 * process type. This function joins those two vectors -- resolving each
 * value's definition id to its parameter name -- and constructs the
 * matching ores.analytics.quant process with its strongly-typed params
 * struct.
 *
 * @param process_type The process type code, e.g. "VASICEK" or
 *        "TWO_FACTOR_GAUSSIAN". Matched case-insensitively against the
 *        definitions' process_type_code.
 * @param definitions The parameter-definition catalogue; only the rows
 *        belonging to @a process_type are used, so callers may pass the
 *        whole table.
 * @param values The parameter-value rows for one config.
 *
 * @throws std::invalid_argument if:
 *   - @a process_type names no known yield-curve process;
 *   - a value row references a definition that is not a @a process_type
 *     parameter (unknown id, or a definition of another process type);
 *   - the same parameter appears more than once;
 *   - any parameter required by @a process_type is missing from
 *     @a values;
 *   - @a values contains a parameter not declared for @a process_type;
 *   - any value lies outside its definition's [min_value, max_value].
 */
ORES_SYNTHETIC_API_EXPORT std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess>
map_parameters_to_yield_curve_process(
    const std::string& process_type,
    const std::vector<yield_curve_process_parameter_definition>& definitions,
    const std::vector<ir_curve_generation_config_process_parameter_value>& values,
    std::uint32_t seed,
    double dt);

} // namespace ores::synthetic::domain

#endif
