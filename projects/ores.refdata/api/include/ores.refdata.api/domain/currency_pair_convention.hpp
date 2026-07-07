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
#ifndef ORES_REFDATA_API_DOMAIN_CURRENCY_PAIR_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_CURRENCY_PAIR_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief Quoting and date conventions for a currency pair, 1:1 with currency_pair.
 *
 * Quoting and date-convention fields for a currency pair — pip factor,
 * tick size, calendars, business day convention, spot-relative/end-of-
 * month flags — folded in from the retired fx_convention entity (see
 * [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]]). Keyed 1:1
 * by pair_code, the same value space as
 * [[id:E1EE950D-FC22-4DAB-A93F-8B2A15196031][ores.refdata.currency_pair]]'s own primary key — every
 * pair has at most one convention record, so a separate identifier scheme would be pure overhead.
 * Like every other soft-FK relationship in this codebase, pair_code is validated via trigger, not a
 * hard DB foreign key.
 */
struct currency_pair_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Same value as the owning currency_pair.pair_code (e.g. "EUR/USD") — a 1:1 extension
     * key, not an independent identifier.
     */
    std::string pair_code;

    /**
     * @brief Converts pips to absolute rate moves (0.0001 for most pairs, 0.01 for JPY crosses).
     */
    double pip_factor;

    /**
     * @brief Minimum rate increment, in pips.
     */
    double tick_size;

    /**
     * @brief Decimal places for rate display.
     */
    int decimal_places = 0;

    /**
     * @brief Calendar used advancing spot→forward. Free-text pending
     * [[id:E1196536-38E8-4365-B0E6-A269F7CA3923][Model calendars as proper ORE Studio reference
     * data]].
     */
    std::optional<std::string> advance_calendar;

    /**
     * @brief Soft FK to ores_trading_business_day_convention_types_tbl.
     */
    std::optional<std::string> business_day_convention;

    /**
     * @brief Whether forward dates are generated relative to the spot date.
     */
    std::optional<bool> spot_relative;

    /**
     * @brief Whether end-of-month convention applies.
     */
    std::optional<bool> end_of_month;

    /**
     * @brief Username of the person who last modified this currency pair convention.
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
