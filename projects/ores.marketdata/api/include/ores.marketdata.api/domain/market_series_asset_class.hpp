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
#ifndef ORES_MARKETDATA_DOMAIN_MARKET_SERIES_ASSET_CLASS_HPP
#define ORES_MARKETDATA_DOMAIN_MARKET_SERIES_ASSET_CLASS_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Links a market series to each asset class it belongs to.
 *
 * A market series is a container of observations, and the asset classes it
 * serves follow from what it observes rather than from a single value on
 * the series itself. Most series belong to exactly one class, but a
 * pairwise correlation relates two -- an equity index against an FX rate,
 * say -- and belongs to both. A not-null column cannot hold that, so the
 * relationship is a junction.
 */
struct market_series_asset_class final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the market series.
     *
     * References ores_marketdata_market_series_tbl.id (soft FK).
     */
    boost::uuids::uuid market_series_id;

    /**
     * @brief Code of an asset class the series belongs to.
     *
     * References ores_refdata_asset_class_codes_tbl.code (soft FK).
     */
    std::string asset_class_code;

    /**
     * @brief Username of the person who last modified this asset class.
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
 * @brief Dispatch-key identifier for market_series_asset_class, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_series_asset_class&) {
    return "ores.marketdata.market_series_asset_class";
}

}

#endif
