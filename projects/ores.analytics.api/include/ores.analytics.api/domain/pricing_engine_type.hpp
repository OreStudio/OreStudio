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
#ifndef ORES_ANALYTICS_DOMAIN_PRICING_ENGINE_TYPE_HPP
#define ORES_ANALYTICS_DOMAIN_PRICING_ENGINE_TYPE_HPP

#include <chrono>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::analytics::domain {

/**
 * @brief Analytics-specific product taxonomy for ORE pricing engines.
 *
 * Maps ORE pricingengine.xml Product/@type values to a normalised registry.
 * Finer-grained than instrument types: a single instrument type (e.g. Swaption)
 * may have multiple engine types (EuropeanSwaption, BermudanSwaption,
 * AmericanSwaption). Coupon/leg-level pricers have no corresponding instrument
 * type (instrument_type_code is empty).
 */
struct pricing_engine_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief ORE pricing engine type code (e.g. 'EuropeanSwaption', 'CMS').
     *
     * Matches the Product/@type attribute in pricingengine.xml.
     */
    std::string code;

    /**
     * @brief Human-readable description of the pricing engine type.
     */
    std::string description;

    /**
     * @brief Optional link to the corresponding instrument type.
     *
     * Soft FK to ores_trading_trade_types_tbl. Empty for coupon/leg-level
     * pricers that have no corresponding booking-level instrument type.
     */
    std::string instrument_type_code;

    /**
     * @brief Username of the person who last modified this record.
     */
    std::string modified_by;

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
