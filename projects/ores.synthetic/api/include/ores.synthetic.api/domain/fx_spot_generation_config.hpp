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
#ifndef ORES_SYNTHETIC_API_DOMAIN_FX_SPOT_GENERATION_CONFIG_HPP
#define ORES_SYNTHETIC_API_DOMAIN_FX_SPOT_GENERATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::synthetic::domain {

/**
 * @brief Configuration for generating synthetic FX spot observations.
 *
 * A typed sub-configuration owned by a market_data_generation_config. Describes
 * how to synthesise the tick stream for a single FX spot rate: which ORE market
 * key it produces, the starting price, the tick cadence, and the price process.
 * The price process is a Gaussian Mixture Model whose components are held
 * separately as gmm_component rows. Scoped to a tenant and a party.
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
     * @brief Base currency ISO code of the FX spot pair (soft FK to currencies).
     */
    std::string base_currency_code;

    /**
     * @brief Quote currency ISO code of the FX spot pair (soft FK to currencies).
     */
    std::string quote_currency_code;

    /**
     * @brief Stable source name carried as provenance of generated observations (e.g.
     * "synthetic.eurusd"). Derived from the currency pair; unique per tenant and party.
     */
    std::string source_name;

    /**
     * @brief ORE market key identifying the FX spot rate produced (e.g. "FX/RATE/EUR/USD"). Derived
     * from the currency pair; not entered directly.
     */
    std::string ore_key;

    /**
     * @brief Where this feed's initial spot price comes from: "fixed" (use gmm_initial_price as
     * entered) or "vintage" (derive it from the vintage_source/vintage_date market-data
     * observation, guarded by availability). Exactly one of gmm_initial_price or
     * vintage_source/vintage_date is populated, matching this discriminator — see the SQL check.
     */
    std::string price_source = "vintage";

    /**
     * @brief Initial spot price the generated process starts from. Only used (and required, > 0)
     * when price_source is "fixed"; 0 when "vintage" — the codegen used here has no true-nullable
     * numeric support, so this is a sentinel enforced by the SQL check alongside price_source, not
     * real SQL NULL.
     */
    double gmm_initial_price = 0.0;

    /**
     * @brief Number of synthetic ticks produced per hour.
     */
    int ticks_per_hour = 0;

    /**
     * @brief Price-process engine: "geometric" (multiplicative), "arithmetic" (additive), or "ou"
     * (Ornstein-Uhlenbeck, mean-reverting).
     */
    std::string process_type = "geometric";

    /**
     * @brief Whether the configuration is active and eligible for generation.
     */
    bool enabled = false;

    /**
     * @brief Source tag of the market-data vintage this feed's initial spot and availability guard
     * are validated against (e.g. "ore.reference"), matching market_observation.source. Only
     * populated (and required) when price_source is "vintage"; empty when "fixed" — see
     * price_source. (Empty string, not SQL NULL: the codegen used here has no true-nullable text
     * support.)
     */
    std::string vintage_source;

    /**
     * @brief Observation date of the market-data vintage this feed's initial spot and availability
     * guard are validated against, ISO format (e.g. "2016-02-05"), matching
     * market_observation.observation_datetime. Only populated (and required) when price_source is
     * "vintage"; empty when "fixed" — see price_source.
     */
    std::string vintage_date;

    /**
     * @brief Username of the person who last modified this FX spot generation config.
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

}

#endif
