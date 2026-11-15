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
#include "ores.analytics.quant/domain/calendar_rule.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using ores::analytics::quant::domain::business_day_convention;
using ores::analytics::quant::domain::observance_shift;
using ores::analytics::quant::domain::period_unit;
using ores::analytics::quant::domain::resolution_algorithm;
using ores::analytics::quant::domain::resolution_status;
using ores::analytics::quant::domain::tenor_anchor;
using ores::analytics::quant::domain::tenor_kind;
using ores::analytics::quant::domain::to_string;

}

TEST_CASE("every resolution status spells itself for a refusal message", "[date_convention]") {
    const resolution_status statuses[] = {resolution_status::ok,
                                          resolution_status::tenor_not_in_convention,
                                          resolution_status::calendar_index_out_of_range,
                                          resolution_status::unsupported_anchor,
                                          resolution_status::missing_spot_date,
                                          resolution_status::period_tenor_without_period,
                                          resolution_status::special_tenor_without_offset,
                                          resolution_status::inconsistent_schedule_declaration,
                                          resolution_status::schedule_without_dates,
                                          resolution_status::schedule_dates_not_ordered,
                                          resolution_status::schedule_exhausted};

    for (const auto status : statuses) {
        CHECK_FALSE(to_string(status).empty());
        CHECK(to_string(status) != "unknown");
    }

    CHECK(to_string(resolution_status::unsupported_anchor) == "unsupported_anchor");
}


TEST_CASE("periods order as QuantLib orders them", "[date_convention]") {
    using ores::analytics::quant::domain::compare_periods;
    using ores::analytics::quant::domain::date_period;
    const auto p = [](period_unit u, int n) {
        return date_period{u, n};
    };

    CHECK(compare_periods(p(period_unit::month, 12), p(period_unit::year, 1)) == 0);
    CHECK(compare_periods(p(period_unit::year, 1), p(period_unit::month, 18)) < 0);
    CHECK(compare_periods(p(period_unit::week, 2), p(period_unit::day, 14)) == 0);
    CHECK(compare_periods(p(period_unit::week, 3), p(period_unit::month, 1)) < 0);
    CHECK(compare_periods(p(period_unit::month, 1), p(period_unit::week, 5)) < 0);
    CHECK(compare_periods(p(period_unit::year, 2), p(period_unit::week, 1)) > 0);
    CHECK(compare_periods(p(period_unit::month, 1), p(period_unit::week, 4)) ==
          std::partial_ordering::unordered);
    CHECK(compare_periods(date_period{}, p(period_unit::day, 1)) ==
          std::partial_ordering::unordered);
    CHECK(compare_periods(date_period{}, date_period{}) == 0);
}
