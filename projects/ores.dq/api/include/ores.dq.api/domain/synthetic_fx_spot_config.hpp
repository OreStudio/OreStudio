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
#ifndef ORES_DQ_API_DOMAIN_SYNTHETIC_FX_SPOT_CONFIG_HPP
#define ORES_DQ_API_DOMAIN_SYNTHETIC_FX_SPOT_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config rows
 * for the synthetic market data bundle
 *
 * Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config rows for the
 * synthetic market data bundle. Each row is a complete FX config combining the parent
 * market_data_generation_config fields (name, description, enabled) with the FX spot sub-config
 * fields (currency pair, initial price, tick cadence, process type).
 */
struct synthetic_fx_spot_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the synthetic FX spot config record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    std::string name;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    std::optional<std::string> description;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    bool enabled = false;

    /**
     * @brief Whether this config starts automatically when the service comes up, as opposed to
     * manual-start-only. Orthogonal to enabled: a config can be enabledtrue, auto_startfalse
     * (valid, manually startable, but never auto-started).
     */
    bool auto_start = false;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    std::string base_currency_code;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    std::string quote_currency_code;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    double gmm_initial_price;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    int ticks_per_hour = 0;

    /**
     * @brief Synthetic FX Spot Config artefacts - denormalized parent+child FX generation config
     * rows for the synthetic market data bundle. Each row is a complete FX config combining the
     * parent market_data_generation_config fields (name, description, enabled) with the FX spot
     * sub-config fields (currency pair, initial price, tick cadence, process type).
     */
    std::string process_type;

    /**
     * @brief Where this row's initial spot price comes from: fixed (use gmm_initial_price as
     * entered) or vintage (derive it from vintage_source/vintage_date, guarded by availability).
     */
    std::string price_source;

    /**
     * @brief Source tag of the market-data vintage this row's initial spot is validated against
     * (e.g. ore.reference, fed.h10.2016-02-05). Only populated when price_source is vintage.
     */
    std::optional<std::string> vintage_source;

    /**
     * @brief Observation date of the market-data vintage this row's initial spot is validated
     * against, ISO format (e.g. 2016-02-05). Only populated when price_source is vintage.
     */
    std::optional<std::string> vintage_date;

    /**
     * @brief Username of the person who last modified this synthetic FX spot config.
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
 * @brief Dispatch-key identifier for synthetic_fx_spot_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const synthetic_fx_spot_config&) {
    return "ores.dq.synthetic_fx_spot_config";
}

}

#endif
