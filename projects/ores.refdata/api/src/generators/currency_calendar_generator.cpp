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
#include "ores.refdata.api/generators/currency_calendar_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <atomic>
#include <string>

namespace ores::refdata::generators {

using ores::utility::generation::generation_keys;

domain::currency_calendar
generate_synthetic_currency_calendar(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(generation_keys::modified_by, "system");
    const auto tenant_id = ctx.env().get_or(generation_keys::tenant_id, "system");

    // The composite key is (currency_iso_code, calendar_code), and
    // neither field carries any other per-call-unique data (unlike e.g.
    // party_country's randomly generated party_id) -- an incrementing
    // suffix on the currency code is the only way to guarantee that
    // repeated calls (generate_synthetic_currency_calendars) don't
    // collide on the same key.
    static std::atomic<std::size_t> counter{0};
    const auto suffix = std::to_string(++counter);

    domain::currency_calendar r;
    r.version = 1;
    r.tenant_id = utility::uuid::tenant_id::from_string(tenant_id).value_or(
        utility::uuid::tenant_id::system());
    r.currency_iso_code = "X" + suffix;
    r.calendar_code = "US";
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::currency_calendar>
generate_synthetic_currency_calendars(std::size_t n, utility::generation::generation_context& ctx) {
    std::vector<domain::currency_calendar> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_currency_calendar(ctx));
    return r;
}

std::vector<domain::currency_calendar>
generate_currency_calendars(utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(generation_keys::modified_by, "system");
    const auto tid = ctx.env().get_or(generation_keys::tenant_id, "system");
    const auto parsed_tid = utility::uuid::tenant_id::from_string(tid);
    const auto tenant_id =
        parsed_tid.has_value() ? parsed_tid.value() : utility::uuid::tenant_id::system();
    const auto now = ctx.past_timepoint();

    // Mirrors the holiday_calendar values previously seeded directly on
    // ores_refdata_currencies_tbl (iso_currencies_artefact_populate.sql),
    // now expressed as currency->calendar junction rows.
    struct row {
        const char* currency;
        const char* calendar;
    };
    static constexpr row rows[] = {
        {"USD", "UnitedStates.Settlement"},
        {"CAD", "Canada.Settlement"},
        {"MXN", "Mexico"},
        {"BRL", "Brazil"},
        {"EUR", "TARGET"},
        {"GBP", "UnitedKingdom.Settlement"},
        {"CHF", "Switzerland"},
        {"NOK", "Norway"},
        {"SEK", "Sweden"},
        {"DKK", "Denmark"},
        {"JPY", "Japan"},
        {"CNY", "China.SSE"},
        {"HKD", "HongKong"},
        {"KRW", "SouthKorea.Settlement"},
        {"SGD", "Singapore"},
        {"INR", "India"},
        {"AUD", "Australia"},
        {"NZD", "NewZealand"},
        {"ZAR", "SouthAfrica"},
    };

    std::vector<domain::currency_calendar> result;
    result.reserve(std::size(rows));
    for (const auto& r : rows) {
        result.push_back({.tenant_id = tenant_id,
                          .currency_iso_code = r.currency,
                          .calendar_code = r.calendar,
                          .modified_by = modified_by,
                          .performed_by = modified_by,
                          .change_reason_code = "system.initial_load",
                          .change_commentary = "Currency spot/settlement calendar reference data",
                          .recorded_at = now});
    }
    return result;
}

}
