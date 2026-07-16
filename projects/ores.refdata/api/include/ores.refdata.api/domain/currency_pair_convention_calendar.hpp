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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_PAIR_CONVENTION_CALENDAR_HPP
#define ORES_REFDATA_DOMAIN_CURRENCY_PAIR_CONVENTION_CALENDAR_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Links a currency pair convention to the calendar(s) used to advance its spot/maturity
 * dates.
 *
 * Many-to-many junction between currency_pair_convention and calendar:
 * [[id:7316C20B-1C0D-4BE3-A994-1957C7595771][a convention's AdvanceCalendar]]
 * almost always names the union of both legs' calendars (e.g.
 * TARGET,UnitedKingdom), so a convention needs more than one calendar
 * row. Replaces currency_pair_convention.advance_calendar's
 * comma-joined free-text value with a proper FK relationship to
 * [[id:1F43998E-BD20-4CBC-8C05-509F1ABF0E0C][the calendar entity]]. The
 * advance_calendar name is kept (not renamed to calendar/holiday_calendar)
 * since it matches ORE's own AdvanceCalendar XML field verbatim and
 * denotes a distinct concept from currency.holiday_calendar — it drives
 * date-advancing (Calendar::advance()), not business/settlement-day
 * membership tests.
 */
struct currency_pair_convention_calendar final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Currency pair code (e.g. EURGBP).
     *
     * References ores_refdata_currency_pair_conventions_tbl.pair_code (soft FK).

The composite key is (pair_code, calendar_code), and neither field carries any other per-call-unique
data -- an inline counter is the only way to guarantee repeated synthetic-generation calls don't
collide on the same key (same convention as currency_country's own synthetic generator).
     */
    std::string pair_code;

    /**
     * @brief QuantLib/ORE calendar token (e.g. TARGET, UnitedKingdom).
     *
     * References ores_refdata_calendars_tbl.code (soft FK).
     */
    std::string calendar_code;

    /**
     * @brief Username of the person who last modified this currency pair convention calendar.
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
 * @brief Dispatch-key identifier for currency_pair_convention_calendar, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const currency_pair_convention_calendar&) {
    return "ores.refdata.currency_pair_convention_calendar";
}

}

#endif
