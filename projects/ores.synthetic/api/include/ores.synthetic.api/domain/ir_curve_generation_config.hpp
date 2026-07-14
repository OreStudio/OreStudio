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
#ifndef ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_API_DOMAIN_IR_CURVE_GENERATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief Configuration for generating a synthetic IR curve from a short-rate process.
 *
 * A typed sub-configuration owned by a market_data_generation_config. Describes
 * how to synthesise a single currency+index interest-rate curve: which
 * short-rate process drives it (vasicek/cir/hull_white, per
 * ores.analytics.quant's IYieldCurveProcess engines) and that process's
 * parameters. The actual instrument grid (which tenors to publish at which
 * role) is held separately as ordered ir_curve_template_entry rows, the
 * same one-config-many-children shape fx_spot_generation_config uses for
 * its gmm_component rows. Scoped to a tenant and a party.
 */
struct ir_curve_generation_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this configuration belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent market_data_generation_config this sub-config belongs to.
     */
    boost::uuids::uuid config_id;

    /**
     * @brief Currency this curve is denominated in (soft FK to currencies).
     */
    std::string currency_code;

    /**
     * @brief Floating-rate index this curve represents (e.g. "SOFR", "ESTR", "SONIA"). Free text:
     * the universe of index names is currency/convention-specific and not yet its own refdata
     * catalogue.
     */
    std::string index_name;

    /**
     * @brief Short-rate process engine driving this curve: "vasicek", "cir", or "hull_white" --
     * selects among ores.analytics.quant's IYieldCurveProcess engines via process_factory::
     * make_yield_curve_process().
     */
    std::string process_type = "vasicek";

    /**
     * @brief Mean-reversion speed parameter.
     */
    double kappa = 0.0;

    /**
     * @brief Constant mean-reversion target level. Persisted as a single scalar, not a full
     * time-varying theta_path: fitting theta(t) to reproduce an observed market curve exactly is
     * out of scope (needs curve bootstrapping, a separate future story) -- a constant theta still
     * drives a real, internally-consistent latent curve via each engine's discount_factor(), just
     * not one fitted to today's market.
     */
    double theta = 0.0;

    /**
     * @brief Volatility parameter.
     */
    double sigma = 0.0;

    /**
     * @brief Starting short rate the process simulates from.
     */
    double initial_rate = 0.0;

    /**
     * @brief Number of synthetic ticks (curve publications) produced per hour.
     */
    int ticks_per_hour = 0;

    /**
     * @brief Whether the configuration is active and eligible for generation.
     */
    bool enabled = false;

    /**
     * @brief Username of the person who last modified this IR curve generation config.
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
 * @brief Dispatch-key identifier for ir_curve_generation_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ir_curve_generation_config&) {
    return "ores.synthetic.ir_curve_generation_config";
}

}

#endif
