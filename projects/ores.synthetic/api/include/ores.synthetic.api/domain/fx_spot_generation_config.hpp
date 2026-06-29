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
#ifndef ORES_SYNTHETIC_DOMAIN_FX_SPOT_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_DOMAIN_FX_SPOT_GENERATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::synthetic::domain {

/**
 * @brief Configuration for generating synthetic FX spot observations.
 *
 * A typed sub-configuration owned by a market_data_generation_config. It
 * describes how to synthesise the tick stream for a single FX spot rate: which
 * ORE market key it produces, the starting price, the tick cadence, and the
 * source name carried as provenance on each generated observation.
 *
 * The price process is a Gaussian Mixture Model whose components are held
 * separately as gmm_component rows that reference this configuration.
 *
 * Scoped to a tenant and a party so each party manages its own configurations
 * and its own generated data.
 */
struct fx_spot_generation_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for this configuration.
     */
    boost::uuids::uuid id{};

    /**
     * @brief Owning party; the configuration and the data it generates belong
     * to this party within the tenant.
     */
    boost::uuids::uuid party_id{};

    /**
     * @brief Owning market data generation config.
     *
     * References the market_data_generation_configs table (soft FK).
     */
    boost::uuids::uuid config_id{};

    /**
     * @brief Base currency ISO code of the FX spot pair (soft FK to
     * currencies). Source of truth selected by the user; the ORE key is
     * derived from it.
     */
    std::string base_currency_code;

    /**
     * @brief Quote currency ISO code of the FX spot pair (soft FK to
     * currencies). Source of truth selected by the user; the ORE key is
     * derived from it.
     */
    std::string quote_currency_code;

    /**
     * @brief Stable source name carried as the provenance of generated
     * observations (e.g. "synthetic.eurusd"). Unique per tenant and party.
     *
     * Derived from the currency pair; not free-text.
     */
    std::string source_name;

    /**
     * @brief ORE market key identifying the FX spot rate produced.
     *
     * Derived from the currency pair as "FX/RATE/<base>/<quote>" (e.g.
     * "FX/RATE/EUR/USD"); not entered directly.
     */
    std::string ore_key;

    /**
     * @brief Initial spot price the generated process starts from.
     */
    double gmm_initial_price = 0.0;

    /**
     * @brief Number of synthetic ticks produced per hour.
     */
    int ticks_per_hour = 0;

    /**
     * @brief Price-process engine that applies the GMM increment to the price.
     *
     * "geometric" — multiplicative (price *= exp(increment)); geometric
     * Brownian motion / log-returns. "arithmetic" — additive (price +=
     * increment); arithmetic Brownian motion. Defaults to geometric.
     */
    std::string process_type = "geometric";

    /**
     * @brief Whether the configuration is active and eligible for generation.
     */
    bool enabled = false;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string modified_by;

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
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
