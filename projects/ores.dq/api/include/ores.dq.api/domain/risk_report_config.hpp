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
#ifndef ORES_DQ_API_DOMAIN_RISK_REPORT_CONFIG_HPP
#define ORES_DQ_API_DOMAIN_RISK_REPORT_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Risk report configuration artefacts - the run settings a seeded report definition resolves
 *
 * Risk report configuration artefacts - the run settings a seeded report
 * definition resolves.
 */
struct risk_report_config final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the staged configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief The name of the report definition this configuration belongs to. It is the join key
     * the publication uses, so it must match the definition exactly.
     */
    std::string report_name;

    /**
     * @brief The currency the run aggregates to.
     */
    std::string base_currency;

    /**
     * @brief ORE's observation model, one of disable, none, move or defer.
     */
    std::string observation_model;

    /**
     * @brief Threads the engine may use.
     */
    int n_threads = 0;

    /**
     * @brief Where the run reads its market data, one of live, eod or date.
     */
    std::string market_data_type;

    /**
     * @brief Whether the run prices the portfolio.
     */
    int npv_enabled = 0;

    /**
     * @brief Whether the run projects cashflows.
     */
    int cashflow_enabled = 0;

    /**
     * @brief Whether the run reports the bootstrapped curves.
     */
    int curves_enabled = 0;

    /**
     * @brief Whether the run computes sensitivities.
     */
    int sensitivity_enabled = 0;

    /**
     * @brief Whether the run simulates risk factors.
     */
    int simulation_enabled = 0;

    /**
     * @brief Whether the run computes valuation adjustments.
     */
    int xva_enabled = 0;

    /**
     * @brief Whether the run applies stress scenarios.
     */
    int stress_enabled = 0;

    /**
     * @brief Whether the run computes a parametric value at risk.
     */
    int parametric_var_enabled = 0;

    /**
     * @brief Whether the run computes initial margin.
     */
    int initial_margin_enabled = 0;

    /**
     * @brief Whether the run computes potential future exposure.
     */
    int pfe_enabled = 0;

    /**
     * @brief Whether the run computes credit valuation adjustment.
     */
    int xva_cva_enabled = 0;

    /**
     * @brief Whether the run computes debit valuation adjustment.
     */
    int xva_dva_enabled = 0;

    /**
     * @brief Whether the run computes funding valuation adjustment.
     */
    int xva_fva_enabled = 0;

    /**
     * @brief The order the seeded configurations are written in.
     */
    int display_order = 0;

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
    friend bool operator==(const risk_report_config&, const risk_report_config&) = default;
};

/**
 * @brief Dispatch-key identifier for risk_report_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const risk_report_config&) {
    return "ores.dq.risk_report_config";
}

}

#endif
