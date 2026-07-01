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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_SERIES_HPP

#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/series_subclass.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief Catalog entry identifying what is being observed (series type, metric, qualifier, asset
 * class).
 *
 * A catalog entry for a market data series — it records what is being observed:
 * a yield curve, vol surface, spot rate, fixing index, or similar. Standard
 * temporal reference data; changes infrequently so a regular table with GIST
 * exclusion is appropriate.
 *
 * Every ORE market data key follows the skeleton TYPE / METRIC / QUALIFIER;
 * asset_class and series_subclass carry the coarse taxonomy for filtering.
 */
struct market_series final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this market series.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this market series.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief ORE market data type token (e.g. FXSpot, YieldCurve, FXVolatility).
     */
    std::string series_type;

    /**
     * @brief Metric within the series type (e.g. SPOT, DISCOUNT, FLAT_FWD_VOLATILITY).
     */
    std::string metric;

    /**
     * @brief Free-text qualifier disambiguating the series within type+metric (e.g. EUR,
     * EUR-EURIBOR-3M, or an empty string for scalars).
     */
    std::string qualifier;

    /**
     * @brief Coarse asset class taxonomy: FX, RATES, CREDIT, EQUITY, COMMODITY, INFLATION, BOND,
     * CROSS_ASSET.
     */
    domain::asset_class asset_class;

    /**
     * @brief Subclass within the asset class (e.g. SPOT, VOLATILITY, YIELD, SPREAD).
     */
    domain::series_subclass series_subclass;

    /**
     * @brief True when the series has no point dimension (e.g. an FX spot rate or a single fixing),
     * false when it is curve/surface/matrix data.
     */
    bool is_scalar = false;

    /**
     * @brief Username of the person who last modified this market series.
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
