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
#include "ores.reporting.core/service/run_document_generator.hpp"
#include <algorithm>
#include <optional>

namespace ores::reporting::service {

using domain::report_run_setup;
using domain::risk_report_config;

namespace {

struct switch_rule {
    int risk_report_config::*enabled;
    const char* name;
    std::optional<const char*> analytic_type;
};

// In the order ORE runs the analytics. The value-at-risk switch has no analytic
// type in the vocabulary, so it has no entry to write.
constexpr switch_rule switch_rules[] = {
    {&risk_report_config::npv_enabled, "npv", "npv"},
    {&risk_report_config::cashflow_enabled, "cashflow", "cashflow"},
    {&risk_report_config::curves_enabled, "curves", "curves"},
    {&risk_report_config::sensitivity_enabled, "sensitivity", "sensitivity"},
    {&risk_report_config::stress_enabled, "stress", "stress"},
    {&risk_report_config::simulation_enabled, "simulation", "simulation"},
    {&risk_report_config::xva_enabled, "xva", "xva"},
    {&risk_report_config::pfe_enabled, "pfe", "pfe"},
    {&risk_report_config::initial_margin_enabled, "initial_margin", "simm"},
    {&risk_report_config::parametric_var_enabled, "parametric_var", std::nullopt},
};

struct input_file {
    const char* type_code;
    std::optional<std::string> report_run_setup::*member;
    const char* file;
};

constexpr input_file input_files[] = {
    {"conventions", &report_run_setup::conventions_file, "conventions.xml"},
    {"curve_configuration", &report_run_setup::curve_config_file, "curveconfig.xml"},
    {"todays_market", &report_run_setup::market_config_file, "todaysmarket.xml"},
    {"pricing_engines", &report_run_setup::pricing_engines_file, "pricingengine.xml"},
};

}

std::vector<std::string> enabled_analytic_types(const risk_report_config& risk) {
    std::vector<std::string> r;
    for (const auto& rule : switch_rules)
        if (risk.*rule.enabled != 0 && rule.analytic_type)
            r.emplace_back(*rule.analytic_type);
    return r;
}

std::vector<std::string> unmapped_switches(const risk_report_config& risk) {
    std::vector<std::string> r;
    for (const auto& rule : switch_rules)
        if (risk.*rule.enabled != 0 && !rule.analytic_type)
            r.emplace_back(rule.name);
    return r;
}

messaging::run_document generate_run_document(const run_document_inputs& in) {
    messaging::run_document r;

    auto& setup = r.setup;
    setup.report_definition_id = in.report_definition_id;
    setup.party_id = in.party_id;
    if (!in.asof_date.empty())
        setup.asof_date = in.asof_date;
    setup.input_path = "Input";
    setup.output_path = "Output";
    setup.base_currency = in.risk.base_currency;
    setup.observation_model = in.risk.observation_model;
    setup.n_threads = in.risk.n_threads;
    for (const auto& f : input_files) {
        const auto bound = std::ranges::find(in.bound_types, f.type_code) != in.bound_types.end();
        if (bound)
            setup.*f.member = f.file;
    }

    int order = 0;
    for (const auto& type : enabled_analytic_types(in.risk)) {
        messaging::run_analytic a;
        a.analytic.party_id = in.party_id;
        a.analytic.report_definition_id = in.report_definition_id;
        a.analytic.analytic_type_code = type;
        a.analytic.display_order = ++order;
        a.analytic.active = "Y";
        r.analytics.push_back(std::move(a));
    }

    domain::report_market_binding market;
    market.party_id = in.party_id;
    market.report_definition_id = in.report_definition_id;
    market.role = "pricing";
    market.configuration_name = in.market_configuration;
    market.position = 1;
    r.market_bindings.push_back(std::move(market));

    return r;
}

}
