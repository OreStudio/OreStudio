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

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/series_subclass.hpp"

namespace ores::marketdata::domain {

/**
 * @brief Catalog entry for a market data series.
 *
 * A market series identifies what is being observed — a yield curve, a vol
 * surface, a spot price — independently of when observations were recorded.
 * The ORE key is decomposed into (series_type, metric, qualifier); the
 * full key is always reconstructable as series_type/metric/qualifier.
 *
 * This is a standard temporal entity with full audit trail and GIST exclusion
 * enforcing at most one current record per (tenant, series_type, metric,
 * qualifier).
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
     * @brief Unique identifier for this market series.
     */
    boost::uuids::uuid id{};

    /**
     * @brief ORE key type component (e.g. DISCOUNT, MM, FX, SWAPTION).
     *
     * Corresponds to the first segment of the ORE market data key.
     */
    std::string series_type;

    /**
     * @brief ORE key metric component (e.g. RATE, PRICE, RATE_LNVOL).
     *
     * Corresponds to the second segment of the ORE market data key.
     */
    std::string metric;

    /**
     * @brief Type-specific qualifier (e.g. EUR, EUR/USD, CPTY_A/SR/USD).
     *
     * Groups all tenors/points of a series. The split between qualifier and
     * point_id is type-specific and defined in the C++ series key registry
     * (Phase 3). Stored as free text; always reconstructable.
     */
    std::string qualifier;

    /**
     * @brief Top-level asset class for efficient slice queries.
     */
    domain::asset_class asset_class = domain::asset_class::rates;

    /**
     * @brief Fine-grained subclass within the asset class.
     */
    domain::series_subclass subclass = domain::series_subclass::yield;

    /**
     * @brief True for series with no tenor dimension (e.g. FX spot, equity spot).
     *
     * Scalar series have no point_id on their observations.
     */
    bool is_scalar = false;

    /**
     * @brief Username of the person who last modified this record.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
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
