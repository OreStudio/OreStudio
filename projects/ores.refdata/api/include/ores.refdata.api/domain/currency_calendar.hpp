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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_CALENDAR_HPP
#define ORES_REFDATA_DOMAIN_CURRENCY_CALENDAR_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Links a currency to a holiday calendar used for its spot/settlement date computation.
 *
 * Many-to-many junction between currency and calendar: a currency's
 * spot/settlement date computation can depend on more than one calendar
 * (e.g. a country calendar and a financial-centre calendar). Replaces
 * [[id:E1196536-38E8-4365-B0E6-A269F7CA3923][currency's]] free-text
 * holiday_calendar column with a proper FK relationship to
 * [[id:1F43998E-BD20-4CBC-8C05-509F1ABF0E0C][the calendar entity]].
 */
struct currency_calendar final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ISO 4217 alpha-3 code of the currency.
     *
     * References ores_refdata_currencies_tbl.iso_code (soft FK).
     */
    std::string currency_iso_code;

    /**
     * @brief QuantLib/ORE calendar token (e.g. TARGET, UnitedStates).
     *
     * References ores_refdata_calendars_tbl.code (soft FK).
     */
    std::string calendar_code;

    /**
     * @brief Username of the person who last modified this currency calendar.
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
 * @brief Dispatch-key identifier for currency_calendar, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const currency_calendar&) {
    return "ores.refdata.currency_calendar";
}

}

#endif
