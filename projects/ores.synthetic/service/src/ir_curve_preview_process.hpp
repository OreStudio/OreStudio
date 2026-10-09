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
#ifndef ORES_SYNTHETIC_SERVICE_IR_CURVE_PREVIEW_PROCESS_HPP
#define ORES_SYNTHETIC_SERVICE_IR_CURVE_PREVIEW_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Build the short-rate process from a stateless preview request's named
 * parameters.
 *
 * The request carries parameter names themselves (the UI has the definitions on
 * screen already), so re-materialise the {definition, value} shape the mapping
 * layer consumes on the fly -- with no min/max bounds, which only the UI spin
 * boxes enforce in this path. The mapping layer handles the
 * uppercase-to-lowercase dispatch and throws a clear message on
 * missing/unexpected parameters.
 *
 * Shared by the IR curve's simulate registration and the curve-shape preview,
 * so the two cannot disagree on how a preview request becomes a process.
 */
inline std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess>
make_preview_process(const std::string& process_type,
                     const std::vector<ores::synthetic::messaging::parameter_spec>& parameters,
                     std::uint32_t seed) {
    using namespace ores::synthetic::domain;
    std::vector<yield_curve_process_parameter_definition> definitions;
    std::vector<ir_curve_generation_config_process_parameter_value> values;
    definitions.reserve(parameters.size());
    values.reserve(parameters.size());
    for (const auto& p : parameters) {
        const auto id = boost::uuids::random_generator()();
        yield_curve_process_parameter_definition d;
        d.process_type_code = process_type;
        d.parameter_name = p.parameter_name;
        d.id = id;
        definitions.push_back(std::move(d));
        ir_curve_generation_config_process_parameter_value v;
        v.parameter_definition_id = id;
        v.parameter_value = p.parameter_value;
        values.push_back(std::move(v));
    }
    return map_parameters_to_yield_curve_process(
        process_type, definitions, values, seed, ores::synthetic::feed::ir_curve_feed_dt);
}

}

#endif
