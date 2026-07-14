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
#include "ores.refdata.api/generators/calendar_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::calendar generate_synthetic_calendar(utility::generation::generation_context& ctx) {
    static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::calendar r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.code = std::string("X") + "-" + std::to_string(idx);
    r.name = "Test Calendar " + std::to_string(faker::number::integer(1000, 9999)) + "-" +
             std::to_string(idx);
    r.calendar_type = std::string("public_holiday");
    r.country_code = std::string("ZZ");
    r.image_id = std::nullopt;
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::calendar>
generate_synthetic_calendars(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::calendar> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_calendar(ctx));
    return r;
}

std::vector<domain::calendar>
generate_quantlib_calendars(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(generation_keys::modified_by, "system");
    const auto tid = ctx.env().get_or(generation_keys::tenant_id, "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);
    const auto tenant_id =
        parsed_tid.has_value() ? parsed_tid.value() : utility::uuid::tenant_id::system();
    const auto now = ctx.past_timepoint();

    struct row {
        const char* code;
        const char* name;
        const char* type;
        const char* country;
    };
    static constexpr row rows[] = {
        // Supranational
        {"TARGET", "TARGET (Euro area)", "public_holiday", "ZZ"},
        {"WeekendsOnly", "Weekends Only", "public_holiday", "ZZ"},
        // Single-market national calendars
        {"Argentina", "Argentina", "public_holiday", "AR"},
        {"Australia", "Australia", "public_holiday", "AU"},
        {"Austria", "Austria", "public_holiday", "AT"},
        {"Botswana", "Botswana", "public_holiday", "BW"},
        {"Brazil", "Brazil", "public_holiday", "BR"},
        {"Chile", "Chile", "public_holiday", "CL"},
        {"CzechRepublic", "Czech Republic", "public_holiday", "CZ"},
        {"Denmark", "Denmark", "public_holiday", "DK"},
        {"Finland", "Finland", "public_holiday", "FI"},
        {"France", "France", "public_holiday", "FR"},
        {"HongKong", "Hong Kong", "public_holiday", "HK"},
        {"Hungary", "Hungary", "public_holiday", "HU"},
        {"Iceland", "Iceland", "public_holiday", "IS"},
        {"India", "India (National Stock Exchange)", "financial_centre", "IN"},
        {"Italy", "Italy", "public_holiday", "IT"},
        {"Japan", "Japan", "public_holiday", "JP"},
        {"Mexico", "Mexico", "public_holiday", "MX"},
        {"NewZealand", "New Zealand", "public_holiday", "NZ"},
        {"Norway", "Norway", "public_holiday", "NO"},
        {"Poland", "Poland", "public_holiday", "PL"},
        {"Romania", "Romania", "public_holiday", "RO"},
        {"Russia", "Russia", "public_holiday", "RU"},
        {"SaudiArabia", "Saudi Arabia", "public_holiday", "SA"},
        {"Singapore", "Singapore", "public_holiday", "SG"},
        {"Slovakia", "Slovakia", "public_holiday", "SK"},
        {"SouthAfrica", "South Africa", "public_holiday", "ZA"},
        {"Sweden", "Sweden", "public_holiday", "SE"},
        {"Switzerland", "Switzerland", "public_holiday", "CH"},
        {"Taiwan", "Taiwan", "public_holiday", "TW"},
        {"Thailand", "Thailand", "public_holiday", "TH"},
        {"Turkey", "Turkey", "public_holiday", "TR"},
        {"Ukraine", "Ukraine", "public_holiday", "UA"},
        // United States (Market enum)
        {"UnitedStates.Settlement", "United States (Settlement)", "public_holiday", "US"},
        {"UnitedStates.NYSE", "United States (NYSE)", "financial_centre", "US"},
        {"UnitedStates.GovernmentBond",
         "United States (Government Bond)",
         "financial_centre",
         "US"},
        {"UnitedStates.NERC", "United States (NERC)", "financial_centre", "US"},
        {"UnitedStates.LiborImpact", "United States (Libor Impact)", "financial_centre", "US"},
        {"UnitedStates.FederalReserve",
         "United States (Federal Reserve)",
         "financial_centre",
         "US"},
        {"UnitedStates.SOFR", "United States (SOFR)", "financial_centre", "US"},
        // United Kingdom (Market enum)
        {"UnitedKingdom.Settlement", "United Kingdom (Settlement)", "public_holiday", "GB"},
        {"UnitedKingdom.Exchange", "United Kingdom (Exchange)", "financial_centre", "GB"},
        {"UnitedKingdom.Metals", "United Kingdom (Metals)", "financial_centre", "GB"},
        // China (Market enum)
        {"China.SSE", "China (Shanghai Stock Exchange)", "financial_centre", "CN"},
        {"China.IB", "China (Interbank)", "financial_centre", "CN"},
        // Germany (Market enum)
        {"Germany.Settlement", "Germany (Settlement)", "public_holiday", "DE"},
        {"Germany.FrankfurtStockExchange",
         "Germany (Frankfurt Stock Exchange)",
         "financial_centre",
         "DE"},
        {"Germany.Xetra", "Germany (Xetra)", "financial_centre", "DE"},
        {"Germany.Eurex", "Germany (Eurex)", "financial_centre", "DE"},
        {"Germany.Euwax", "Germany (Euwax)", "financial_centre", "DE"},
        // Canada (Market enum)
        {"Canada.Settlement", "Canada (Settlement)", "public_holiday", "CA"},
        {"Canada.TSX", "Canada (Toronto Stock Exchange)", "financial_centre", "CA"},
        // South Korea (Market enum)
        {"SouthKorea.Settlement", "South Korea (Settlement)", "public_holiday", "KR"},
        {"SouthKorea.KRX", "South Korea (Korea Exchange)", "financial_centre", "KR"},
        // Indonesia (Market enum)
        {"Indonesia.BEJ", "Indonesia (Jakarta SE, legacy BEJ)", "financial_centre", "ID"},
        {"Indonesia.JSX", "Indonesia (Jakarta SE, legacy JSX)", "financial_centre", "ID"},
        {"Indonesia.IDX", "Indonesia (Indonesia Stock Exchange)", "financial_centre", "ID"},
        // Israel (Market enum)
        {"Israel.Settlement", "Israel (Settlement)", "public_holiday", "IL"},
        {"Israel.TASE", "Israel (Tel-Aviv Stock Exchange)", "financial_centre", "IL"},
    };

    std::vector<domain::calendar> result;
    result.reserve(std::size(rows));
    for (const auto& r : rows) {
        result.push_back({.tenant_id = tenant_id,
                          .code = r.code,
                          .name = r.name,
                          .calendar_type = r.type,
                          .country_code = r.country,
                          .modified_by = modified_by,
                          .change_reason_code = "system.initial_load",
                          .change_commentary = "QuantLib calendar reference data",
                          .recorded_at = now});
    }
    return result;
}
}
