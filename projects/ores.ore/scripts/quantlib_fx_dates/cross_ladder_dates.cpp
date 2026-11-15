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
// Prints the spot and value dates QuantLib gives the cross cases that
// fx_forward_ladder_plan_tests.cpp pins, with each calendar's holidays in the
// window, as C++ the test pastes in. A cross advances on its two currencies'
// calendars only, as ORE's FX conventions do (EUR-GBP-FX-CONVENTIONS in
// Examples/Input/conventions.xml advances on TARGET,UK), so a USD holiday does
// not move it.
#include <ql/time/calendars/all.hpp>
#include <ql/time/calendars/jointcalendar.hpp>
#include <cstdio>
#include <string>
#include <vector>

using namespace QuantLib;

namespace {

struct named_calendar {
    std::string name;
    Calendar calendar;
};

struct cross_case {
    std::string pair;
    named_calendar first;
    named_calendar second;
    Date as_of;
};

std::string cpp(const Date& d) {
    char out[64];
    std::snprintf(
        out, sizeof(out), "%dy / %d / %dd", d.year(), static_cast<int>(d.month()), d.dayOfMonth());
    return out;
}

}

int main() {
    const named_calendar target{"TARGET", TARGET()};
    const named_calendar uk{"UnitedKingdom.Settlement", UnitedKingdom(UnitedKingdom::Settlement)};
    const named_calendar japan{"Japan", Japan()};
    const named_calendar australia{"Australia", Australia()};
    const named_calendar new_zealand{"NewZealand", NewZealand()};
    const std::vector<cross_case> cases{
        {"EUR/GBP", target, uk, Date(24, November, 2026)},
        {"EUR/GBP", target, uk, Date(23, December, 2026)},
        {"EUR/JPY", target, japan, Date(29, December, 2026)},
        {"AUD/NZD", australia, new_zealand, Date(22, January, 2027)},
    };
    const std::vector<std::pair<std::string, Period>> tenors{{"1W", Period(1, Weeks)},
                                                             {"1M", Period(1, Months)},
                                                             {"3M", Period(3, Months)},
                                                             {"1Y", Period(1, Years)}};

    for (const auto& c : cases) {
        const JointCalendar joint(c.first.calendar, c.second.calendar, JoinHolidays);
        const Date window_end = c.as_of + Period(13, Months);
        std::printf("// %s as of %s, on %s and %s.\n",
                    c.pair.c_str(),
                    cpp(c.as_of).c_str(),
                    c.first.name.c_str(),
                    c.second.name.c_str());
        for (const auto* cal : {&c.first, &c.second}) {
            std::printf("//   %s holidays:", cal->name.c_str());
            for (const auto& h : cal->calendar.holidayList(c.as_of, window_end, false))
                std::printf(" %s;", cpp(h).c_str());
            std::printf("\n");
        }
        const Date spot = joint.advance(c.as_of, 2, Days);
        std::printf(
            "//   spot %s, SN %s", cpp(spot).c_str(), cpp(joint.advance(spot, 1, Days)).c_str());
        for (const auto& [id, p] : tenors)
            std::printf(", %s %s",
                        id.c_str(),
                        cpp(joint.advance(spot, p, ModifiedFollowing, false)).c_str());
        std::printf("\n");
    }
    return 0;
}
