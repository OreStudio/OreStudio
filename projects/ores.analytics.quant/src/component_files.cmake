# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(files
    "domain/business_day_calendar_set.cpp"
    "domain/process_parameter_validation.cpp"
    "service/calendar_rule_engine.cpp"
    "service/curve_bootstrap_engine.cpp"
    "service/curve_instrument_pricer.cpp"
    "service/day_count_calculator.cpp"
    "service/forward_rate_calculator.cpp"
    "service/process_factory.cpp"
    "service/processes/arithmetic_gaussian_mixture_model_process.cpp"
    "service/processes/cox_ingersoll_ross_process.cpp"
    "service/processes/g2pp_process.cpp"
    "service/processes/gaussian_mixture_model_process.cpp"
    "service/processes/hull_white_process.cpp"
    "service/processes/ornstein_uhlenbeck_process.cpp"
    "service/processes/vasicek_process.cpp"
    "service/quantlib_calendar_rulesets.cpp"
    "service/rate_delta_tracker.cpp"
    "service/rate_engine.cpp"
    "service/rate_reciprocator.cpp"
    "service/risk_recentering.cpp"
    "service/topology_builder.cpp"
)

# Headers must be listed for AUTOMOC to find Q_OBJECT declarations.
set(HEADERS
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/business_day_calendar_set.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/calendar_rule.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/calendar_ruleset.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/ccy_pair.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/ccy_pair_input.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/crm_rate_view.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/crm_topology.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/currency_id.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/derived_rate.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/driver_quote.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/i_stochastic_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/i_yield_curve_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/process_parameter_validation.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/rate_snapshot.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/rate_status.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/staleness_policy.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/topology_build_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/topology_error.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/domain/vertex_state.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/export.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/ores.analytics.quant.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/calendar_rule_engine.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/curve_bootstrap_engine.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/curve_instrument_pricer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/day_count_calculator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/forward_rate_calculator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/process_factory.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/arithmetic_gaussian_mixture_model_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/cox_ingersoll_ross_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/g2pp_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/gaussian_mixture_model_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/hull_white_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/ornstein_uhlenbeck_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/processes/vasicek_process.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/rate_delta_tracker.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/rate_engine.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/rate_reciprocator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/risk_recentering.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/ores.analytics.quant/service/topology_builder.hpp"
)
