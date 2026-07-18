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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_COUNTRY_HPP
#define ORES_REFDATA_DOMAIN_CURRENCY_COUNTRY_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Links a currency to a country that issues it.
 *
 * Many-to-many junction between currency and country: a currency can be
 * issued by more than one country (e.g. EUR spans DE, FR, IT, ES, ...).
 * Independent reference data in its own right, not a prerequisite for
 * [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][the calendar entity]], which
 * carries its own country_code rather than deriving a currency's
 * calendars transitively through this junction.
 */
struct currency_country final {
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
     * @brief ISO 3166-1 alpha-2 code of the country that issues this currency.
     *
     * References ores_refdata_countries_tbl.alpha2_code (soft FK).
     */
    std::string country_alpha2_code;

    /**
     * @brief Username of the person who last modified this currency country.
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
 * @brief Dispatch-key identifier for currency_country, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const currency_country&) {
    return "ores.refdata.currency_country";
}

}

#endif
