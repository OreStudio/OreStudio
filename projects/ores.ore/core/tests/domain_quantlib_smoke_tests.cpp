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
#include <catch2/catch_test_macros.hpp>
#include <ql/time/calendars/target.hpp>
#include <ql/time/calendars/unitedstates.hpp>
#include <ql/time/date.hpp>

TEST_CASE("quantlib_target_recognises_new_years_day", "[quantlib]") {
    const QuantLib::TARGET calendar;
    const QuantLib::Date new_years_day(1, QuantLib::January, 2026);
    REQUIRE(calendar.isHoliday(new_years_day));
}

TEST_CASE("quantlib_united_states_government_bond_holiday_list_is_nonempty", "[quantlib]") {
    const QuantLib::UnitedStates calendar(QuantLib::UnitedStates::GovernmentBond);
    const QuantLib::Date start(1, QuantLib::January, 2026);
    const QuantLib::Date end(31, QuantLib::December, 2026);
    const auto holidays = calendar.holidayList(start, end);
    REQUIRE_FALSE(holidays.empty());
}
