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
#include "ores.synthetic.api/domain/yield_curve_process_parameter_mapping.hpp"
#include "ores.analytics.quant/service/processes/black_karasinski_params.hpp"
#include "ores.analytics.quant/service/processes/black_karasinski_process.hpp"
#include "ores.analytics.quant/service/processes/cox_ingersoll_ross_params.hpp"
#include "ores.analytics.quant/service/processes/cox_ingersoll_ross_process.hpp"
#include "ores.analytics.quant/service/processes/hull_white_params.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/two_factor_gaussian_params.hpp"
#include "ores.analytics.quant/service/processes/two_factor_gaussian_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_params.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <map>
#include <stdexcept>
#include <vector>

namespace ores::synthetic::domain {

namespace {

std::string to_lower(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) { return std::tolower(c); });
    return s;
}

/// Parameter names each process type requires, keyed by the lowercased
/// process type code. The row-based store must contain exactly these.
const std::map<std::string, std::vector<std::string>>& expected_parameters() {
    static const std::map<std::string, std::vector<std::string>> m{
        {"black_karasinski", {"kappa", "theta", "sigma", "initial_rate"}},
        {"vasicek", {"kappa", "theta", "sigma", "initial_rate"}},
        {"cox_ingersoll_ross", {"kappa", "theta", "sigma", "initial_rate"}},
        {"hull_white", {"kappa", "theta", "sigma", "initial_rate"}},
        {"two_factor_gaussian",
         {"kappa_x", "kappa_y", "theta", "sigma_x", "sigma_y", "rho", "initial_rate"}},
    };
    return m;
}

std::string join_quoted(const std::vector<std::string>& v) {
    std::string out;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (i > 0)
            out += (i + 1 == v.size() ? " and " : ", ");
        out += "'" + v[i] + "'";
    }
    return out;
}

} // namespace

std::unique_ptr<ores::analytics::quant::domain::IYieldCurveProcess>
map_parameters_to_yield_curve_process(
    const std::string& process_type,
    const std::vector<yield_curve_process_parameter_definition>& definitions,
    const std::vector<ir_curve_generation_config_process_parameter_value>& values,
    std::uint32_t seed,
    double dt) {

    using ores::analytics::quant::service::black_karasinski_params;
    using ores::analytics::quant::service::black_karasinski_process;
    using ores::analytics::quant::service::cox_ingersoll_ross_params;
    using ores::analytics::quant::service::cox_ingersoll_ross_process;
    using ores::analytics::quant::service::hull_white_params;
    using ores::analytics::quant::service::hull_white_process;
    using ores::analytics::quant::service::two_factor_gaussian_params;
    using ores::analytics::quant::service::two_factor_gaussian_process;
    using ores::analytics::quant::service::vasicek_params;
    using ores::analytics::quant::service::vasicek_process;

    const auto type = to_lower(process_type);
    const auto& expected = expected_parameters();
    const auto expected_it = expected.find(type);
    if (expected_it == expected.end())
        throw std::invalid_argument(
            "map_parameters_to_yield_curve_process: unknown process type '" + process_type + "'");

    // Resolve each definition id to its parameter name, keeping only the
    // definitions that belong to this process type.
    std::map<boost::uuids::uuid, const yield_curve_process_parameter_definition*> definitions_by_id;
    for (const auto& d : definitions) {
        if (to_lower(d.process_type_code) == type)
            definitions_by_id[d.id] = &d;
    }

    // Join the value rows onto the definitions, rejecting rows whose
    // definition is not a parameter of this process type.
    struct named_value {
        double value = 0.0;
        const yield_curve_process_parameter_definition* definition = nullptr;
    };
    std::map<std::string, named_value> named;
    for (const auto& v : values) {
        const auto def_it = definitions_by_id.find(v.parameter_definition_id);
        if (def_it == definitions_by_id.end()) {
            const auto all_it =
                std::find_if(definitions.begin(), definitions.end(), [&](const auto& d) {
                    return d.id == v.parameter_definition_id;
                });
            if (all_it != definitions.end())
                throw std::invalid_argument(
                    "map_parameters_to_yield_curve_process: parameter value references "
                    "parameter_definition_id " +
                    boost::uuids::to_string(v.parameter_definition_id) + " (parameter '" +
                    all_it->parameter_name + "'), which belongs to process type '" +
                    all_it->process_type_code + "', not '" + process_type + "'");
            throw std::invalid_argument(
                "map_parameters_to_yield_curve_process: parameter value references unknown "
                "parameter_definition_id " +
                boost::uuids::to_string(v.parameter_definition_id));
        }
        const auto* const def = def_it->second;
        const auto [jit, inserted] =
            named.emplace(def->parameter_name, named_value{v.parameter_value, def});
        if (!inserted)
            throw std::invalid_argument(
                "map_parameters_to_yield_curve_process: duplicate value for parameter '" +
                def->parameter_name + "' of process type '" + process_type + "'");
    }

    // The named set must match the expected set exactly.
    std::vector<std::string> missing;
    for (const auto& name : expected_it->second) {
        if (!named.count(name))
            missing.push_back(name);
    }
    if (!missing.empty())
        throw std::invalid_argument("map_parameters_to_yield_curve_process: process type '" +
                                    process_type +
                                    "' is missing required parameter(s): " + join_quoted(missing));

    std::vector<std::string> unexpected;
    for (const auto& [name, entry] : named) {
        if (std::find(expected_it->second.begin(), expected_it->second.end(), name) ==
            expected_it->second.end())
            unexpected.push_back(name);
    }
    if (!unexpected.empty())
        throw std::invalid_argument("map_parameters_to_yield_curve_process: process type '" +
                                    process_type +
                                    "' has unexpected parameter(s): " + join_quoted(unexpected));

    // Range-check each value against its definition's bounds.
    for (const auto& [name, entry] : named) {
        const auto& def = *entry.definition;
        if (def.min_value && entry.value < *def.min_value)
            throw std::invalid_argument("map_parameters_to_yield_curve_process: parameter '" +
                                        name + "' of process type '" + process_type +
                                        "' has value " + std::to_string(entry.value) +
                                        ", below its minimum " + std::to_string(*def.min_value));
        if (def.max_value && entry.value > *def.max_value)
            throw std::invalid_argument("map_parameters_to_yield_curve_process: parameter '" +
                                        name + "' of process type '" + process_type +
                                        "' has value " + std::to_string(entry.value) +
                                        ", above its maximum " + std::to_string(*def.max_value));
    }

    const auto value = [&](const std::string& name) {
        return named.at(name).value;
    };

    if (type == "two_factor_gaussian") {
        two_factor_gaussian_params p;
        p.kappa_x = value("kappa_x");
        p.kappa_y = value("kappa_y");
        p.theta = value("theta");
        p.sigma_x = value("sigma_x");
        p.sigma_y = value("sigma_y");
        p.rho = value("rho");
        p.initial_rate = value("initial_rate");
        return std::make_unique<two_factor_gaussian_process>(p, seed, dt);
    }

    if (type == "black_karasinski") {
        black_karasinski_params p;
        p.kappa = value("kappa");
        p.theta = value("theta");
        p.sigma = value("sigma");
        p.initial_rate = value("initial_rate");
        return std::make_unique<black_karasinski_process>(p, seed, dt);
    }

    if (type == "vasicek") {
        vasicek_params p;
        p.kappa = value("kappa");
        p.theta = value("theta");
        p.sigma = value("sigma");
        p.initial_rate = value("initial_rate");
        return std::make_unique<vasicek_process>(p, seed, dt);
    }

    if (type == "cox_ingersoll_ross") {
        cox_ingersoll_ross_params p;
        p.kappa = value("kappa");
        p.theta = value("theta");
        p.sigma = value("sigma");
        p.initial_rate = value("initial_rate");
        return std::make_unique<cox_ingersoll_ross_process>(p, seed, dt);
    }

    // hull_white
    hull_white_params p;
    p.kappa = value("kappa");
    p.theta = value("theta");
    p.sigma = value("sigma");
    p.initial_rate = value("initial_rate");
    return std::make_unique<hull_white_process>(p, seed, dt);
}

} // namespace ores::synthetic::domain
