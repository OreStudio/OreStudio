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
#ifndef ORES_REFDATA_DOMAIN_FX_CONVENTION_HPP
#define ORES_REFDATA_DOMAIN_FX_CONVENTION_HPP

#include <chrono>
#include <string>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for FX spot and forward contracts.
 *
 * Defines the spot settlement lag, pip factor, and adjustment conventions for
 * a currency pair. Corresponds to the <FX> element in ORE conventions.xml.
 * The id typically takes the form 'CCY1-CCY2-FX-CONVENTIONS'.
 */
struct fx_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Unique convention identifier.
     *
     * Examples: 'EUR-USD-FX-CONVENTIONS', 'GBP-USD-FX-CONVENTIONS'.
     */
    std::string id;

    /**
     * @brief Number of business days from trade to spot settlement (usually 2).
     */
    int spot_days = 0;

    /**
     * @brief ISO 4217 code of the source (base) currency (e.g. 'EUR').
     */
    std::string source_currency;

    /**
     * @brief ISO 4217 code of the target (quote) currency (e.g. 'USD').
     */
    std::string target_currency;

    /**
     * @brief Divisor applied to quoted FX points to obtain the rate increment (e.g. 10000.0 for most pairs).
     */
    double points_factor;

    /**
     * @brief Calendar used when advancing from spot to forward dates.
     */
    std::optional<std::string> advance_calendar;

    /**
     * @brief Whether forward dates are generated relative to the spot date.
     */
    std::optional<bool> spot_relative;

    /**
     * @brief Whether end-of-month convention applies.
     */
    std::optional<bool> end_of_month;

    /**
     * @brief Business day convention for forward dates (canonical FpML).
     */
    std::optional<std::string> convention;

    /**
     * @brief Username of the person who last modified this FX convention.
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
