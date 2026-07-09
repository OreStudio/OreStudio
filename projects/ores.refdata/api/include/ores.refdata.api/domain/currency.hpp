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
#ifndef ORES_REFDATA_API_DOMAIN_CURRENCY_HPP
#define ORES_REFDATA_API_DOMAIN_CURRENCY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief ISO 4217 currency definitions.
 *
 * ISO 4217 currency definitions used for reference data.
 * Currencies are managed per-tenant and drive financial calculations,
 * rounding, and display formatting across the system.
 */
struct currency final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief ISO 4217 alpha-3 code (e.g., "USD", "EUR", "GBP").
     *
     * The :name generator block below only supplies the literal "X" prefix consumed by the
     * template's counter-based generate_synthetic_currency codepath (X + counter, e.g. "X0", "X1");
     * the fictional generator (generate_fictional_currencies, injected via custom generator
     * sections below) remains the authoritative test-data source and does not depend on this
     * synthetic path.
     */
    std::string iso_code;

    /**
     * @brief Human-readable currency name (e.g., "US Dollar", "Euro").
     */
    std::string name;

    /**
     * @brief ISO 4217 numeric code (e.g., "840", "978").
     */
    std::string numeric_code;

    /**
     * @brief Currency symbol displayed in the UI (e.g., "$", "€", "£").
     *
     * faker::finance::currencySymbol() can return an empty string for some locales; fall back to
     * "$" rather than violate the non-nullable constraint.
     */
    std::string symbol;

    /**
     * @brief Symbol for the fractional unit (e.g., "¢" for cents, "p" for pence).
     */
    std::string fraction_symbol;

    /**
     * @brief Number of fractional units per whole unit (e.g., 100 for centesimal, 0 for
     * zero-decimal).
     */
    int fractions_per_unit = 0;

    /**
     * @brief Rounding method code; soft FK to ores_refdata_rounding_types_tbl.
     */
    std::string rounding_type;

    /**
     * @brief Number of decimal places for rounding (e.g., 2 for centesimal currencies).
     */
    int rounding_precision = 0;

    /**
     * @brief Boost.Format pattern for displaying amounts (e.g., "%3% %1$.2f").
     */
    std::string format;

    /**
     * @brief Nature classification; soft FK to ores_refdata_monetary_natures_tbl (e.g., "fiat",
     * "commodity").
     */
    std::string monetary_nature;

    /**
     * @brief Market tier classification; soft FK to ores_refdata_currency_market_tiers_tbl (e.g.,
     * "g10", "emerging").
     */
    std::string market_tier;

    /**
     * @brief Optional reference to a flag or logo image in the images table.
     */
    std::optional<boost::uuids::uuid> image_id;

    /**
     * @brief Business days to settlement for a spot trade in this currency (USD is 1, most others
     * are 2). Used to derive currency_pair.spot_days as the max of the two legs' values.
     */
    int spot_days = 0;

    /**
     * @brief Day-count convention code (e.g. "ACT/360", "ACT/365"). Free-text for now — no aux-type
     * table exists yet; see [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in
     * reference data]] for the open question.
     */
    std::string day_basis;

    /**
     * @brief Ordinal rank in the base-currency precedence table used to determine canonical
     * base/quote order for a pair (lower = takes precedence as base currency).
     */
    int base_precedence = 0;

    /**
     * @brief Named holiday calendar identifier (e.g. "TARGET", "US", "UK"). Free-text pending
     * [[id:E1196536-38E8-4365-B0E6-A269F7CA3923][Model calendars as proper ORE Studio reference
     * data]].
     */
    std::optional<std::string> holiday_calendar;

    /**
     * @brief Username of the person who last modified this currency.
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
