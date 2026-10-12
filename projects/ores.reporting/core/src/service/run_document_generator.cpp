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
#include <stdexcept>

namespace ores::reporting::service {

using domain::report_run_setup;

namespace {

struct input_file {
    const char* type_code;
    std::optional<std::string> report_run_setup::* member;
    const char* file;
};

constexpr input_file input_files[] = {
    {"conventions", &report_run_setup::conventions_file, "conventions.xml"},
    {"curve_configuration", &report_run_setup::curve_config_file, "curveconfig.xml"},
    {"todays_market", &report_run_setup::market_config_file, "todaysmarket.xml"},
    {"pricing_engines", &report_run_setup::pricing_engines_file, "pricingengine.xml"},
};

}

messaging::run_document generate_run_document(const run_document_inputs& in) {
    if (std::ranges::none_of(in.analytics, [](const auto& a) { return a.analytic.active == "Y"; }))
        throw std::invalid_argument(
            "The risk report configuration switches no analytic on. An ORE run needs at "
            "least one.");

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

    r.analytics = in.analytics;

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
