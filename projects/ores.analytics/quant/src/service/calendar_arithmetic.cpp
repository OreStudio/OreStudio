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
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include <stdexcept>
#include <string>

namespace ores::analytics::quant::service {

using domain::business_day_calendar_set;
using domain::business_day_convention;
using domain::date_period;
using domain::period_unit;

using namespace std::chrono;

namespace {

void require_known_calendar(const business_day_calendar_set& calendars,
                            std::size_t calendar_index) {
    if (calendar_index >= calendars.calendar_count()) {
        throw std::out_of_range("Calendar index " + std::to_string(calendar_index) +
                                " is not one of the " + std::to_string(calendars.calendar_count()) +
                                " in the set");
    }
}

} // namespace

year_month_day calendar_arithmetic::add_period(year_month_day date, date_period period) {
    const int n = period.multiplier;

    switch (period.unit) {
        case period_unit::day:
            return year_month_day{sys_days{date} + days{n}};
        case period_unit::week:
            return year_month_day{sys_days{date} + weeks{n}};
        case period_unit::month: {
            // The clamp is explicit because `date + months` yields an invalid
            // day when the target month is shorter. It happens once, against
            // the total, so 31 January plus two months is 31 March.
            const auto shifted = date + months{n};
            if (!shifted.ok())
                return year_month_day{year_month_day_last{shifted.year() / shifted.month() / last}};
            return shifted;
        }
        case period_unit::year: {
            const auto shifted = date + years{n};
            if (!shifted.ok())
                return year_month_day{year_month_day_last{shifted.year() / shifted.month() / last}};
            return shifted;
        }
        case period_unit::none:
            throw std::invalid_argument("A period with no unit has no length");
    }
    throw std::invalid_argument("Unrecognised period unit");
}

year_month_day calendar_arithmetic::adjust(year_month_day date,
                                           business_day_convention convention,
                                           std::size_t calendar_index,
                                           const business_day_calendar_set& calendars) {
    if (convention == business_day_convention::unadjusted)
        return date;

    require_known_calendar(calendars, calendar_index);

    const auto is_holiday = [&](year_month_day d) {
        return !calendars.is_business_day(calendar_index, d);
    };
    const auto next_day = [](year_month_day d) {
        return year_month_day{sys_days{d} + days{1}};
    };
    const auto previous_day = [](year_month_day d) {
        return year_month_day{sys_days{d} - days{1}};
    };

    auto candidate = date;
    switch (convention) {
        case business_day_convention::unadjusted:
            return date;

        case business_day_convention::following:
        case business_day_convention::modified_following:
        case business_day_convention::half_month_modified_following: {
            while (is_holiday(candidate))
                candidate = next_day(candidate);
            if (convention == business_day_convention::following)
                return candidate;
            // A modified roll that leaves the month turns back from the date
            // as it was given, not from where the forward walk reached.
            if (candidate.month() != date.month())
                return adjust(date, business_day_convention::preceding, calendar_index, calendars);
            if (convention == business_day_convention::half_month_modified_following &&
                date.day() <= day{15} && candidate.day() > day{15})
                return adjust(date, business_day_convention::preceding, calendar_index, calendars);
            return candidate;
        }

        case business_day_convention::preceding:
        case business_day_convention::modified_preceding: {
            while (is_holiday(candidate))
                candidate = previous_day(candidate);
            if (convention == business_day_convention::preceding)
                return candidate;
            if (candidate.month() != date.month())
                return adjust(date, business_day_convention::following, calendar_index, calendars);
            return candidate;
        }

        case business_day_convention::nearest: {
            auto backwards = date;
            // The two walks move together, so the first side to reach a
            // business day is the nearer one; on a tie the forward walk, which
            // is the one tested first, wins.
            while (is_holiday(candidate) && is_holiday(backwards)) {
                candidate = next_day(candidate);
                backwards = previous_day(backwards);
            }
            return is_holiday(candidate) ? backwards : candidate;
        }
    }
    throw std::invalid_argument("Unrecognised business day convention");
}

year_month_day calendar_arithmetic::advance(year_month_day date,
                                            date_period period,
                                            business_day_convention convention,
                                            bool end_of_month,
                                            std::size_t calendar_index,
                                            const business_day_calendar_set& calendars) {
    if (period.is_empty())
        throw std::invalid_argument("Cannot advance by a period with no unit");

    require_known_calendar(calendars, calendar_index);

    // A zero count still moves the date onto a business day: QuantLib adjusts
    // rather than returning the date untouched.
    if (period.multiplier == 0)
        return adjust(date, convention, calendar_index, calendars);

    if (period.unit == period_unit::day) {
        // A day-counted period walks business days and lands on one, so no
        // convention applies: the walk has already skipped every holiday.
        const auto step = period.multiplier > 0 ? days{1} : days{-1};
        auto d = date;
        for (int remaining = period.multiplier > 0 ? period.multiplier : -period.multiplier;
             remaining > 0;
             --remaining) {
            do {
                d = year_month_day{sys_days{d} + step};
            } while (!calendars.is_business_day(calendar_index, d));
        }
        return d;
    }

    const auto shifted = add_period(date, period);

    // Weeks are calendar time and then adjusted; months and years may snap to
    // the end of the month, and when they do the convention is not consulted
    // at all -- the snap carries its own, which is why an Unadjusted advance
    // under end_of_month still returns a business day.
    if (period.unit != period_unit::week && end_of_month &&
        is_end_of_month(date, calendar_index, calendars)) {
        return last_business_day(
            shifted, business_day_convention::preceding, calendar_index, calendars);
    }

    return adjust(shifted, convention, calendar_index, calendars);
}

bool calendar_arithmetic::is_end_of_month(year_month_day date,
                                          std::size_t calendar_index,
                                          const business_day_calendar_set& calendars) {
    require_known_calendar(calendars, calendar_index);

    // QuantLib asks whether the following day, adjusted by the default
    // Following, lands in a different month.
    const auto next = year_month_day{sys_days{date} + days{1}};
    const auto adjusted =
        adjust(next, business_day_convention::following, calendar_index, calendars);
    return adjusted.month() != date.month();
}

year_month_day calendar_arithmetic::last_business_day(year_month_day date,
                                                      business_day_convention convention,
                                                      std::size_t calendar_index,
                                                      const business_day_calendar_set& calendars) {
    const auto last_day = year_month_day{year_month_day_last{date.year() / date.month() / last}};
    return adjust(last_day, convention, calendar_index, calendars);
}

year_month_day calendar_arithmetic::nth_weekday(unsigned nth, weekday w, month m, year y) {
    if (nth < 1 || nth > 5) {
        throw std::invalid_argument("An nth weekday counts from one to five, not " +
                                    std::to_string(nth));
    }

    const year_month_day first{y / m / day{1}};
    const unsigned first_weekday = weekday{sys_days{first}}.c_encoding();
    const unsigned days_to_first = (w.c_encoding() + 7 - first_weekday) % 7;
    return year_month_day{sys_days{first} + days{static_cast<int>(days_to_first + 7 * (nth - 1))}};
}

year_month_day calendar_arithmetic::imm_date(month m, year y) {
    return nth_weekday(3, Wednesday, m, y);
}

bool calendar_arithmetic::is_imm_date(year_month_day date, bool main_cycle) {
    const auto w = weekday{sys_days{date}};
    const auto d = static_cast<unsigned>(date.day());
    if (w != Wednesday || d < 15 || d > 21)
        return false;
    if (!main_cycle)
        return true;

    const auto m = static_cast<unsigned>(date.month());
    return m == 3 || m == 6 || m == 9 || m == 12;
}

year_month_day calendar_arithmetic::next_imm_date(year_month_day date, bool main_cycle) {
    // A day at a time rather than QuantLib's month-skip arithmetic, which
    // computes the same date: the answer is at most a quarter out, so a loop
    // over it cannot be off by a contract.
    auto candidate = date;
    for (;;) {
        candidate = year_month_day{sys_days{candidate} + days{1}};
        if (is_imm_date(candidate, main_cycle))
            return candidate;
    }
}

} // namespace ores::analytics::quant::service
