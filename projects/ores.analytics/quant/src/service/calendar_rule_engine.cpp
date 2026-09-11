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
#include "ores.analytics.quant/service/calendar_rule_engine.hpp"
#include <map>
#include <optional>
#include <set>

namespace ores::analytics::quant::service {

using domain::calendar_exception;
using domain::calendar_rule;
using domain::calendar_rule_kind;
using domain::calendar_ruleset;
using domain::instantiated_holiday;
using domain::observance_shift;

namespace {

/// Sunday = 0 .. Saturday = 6, matching std::chrono::weekday::c_encoding()
/// and calendar_ruleset::weekend_mask's bit convention.
bool is_weekend(std::chrono::weekday w, const std::bitset<7>& mask) {
    return mask.test(w.c_encoding());
}

std::optional<std::chrono::year_month_day> resolve_natural_date(
    const calendar_rule& rule, std::chrono::year y, std::chrono::year_month_day easter) {
    using namespace std::chrono;

    switch (rule.kind) {
        case calendar_rule_kind::fixed_date: {
            if (!rule.month || !rule.day)
                return std::nullopt;
            return year_month_day{y, *rule.month, std::chrono::day{*rule.day}};
        }
        case calendar_rule_kind::nth_weekday_of_month: {
            if (!rule.month || !rule.weekday || !rule.occurrence)
                return std::nullopt;
            const year_month_weekday ymw{y, *rule.month, (*rule.weekday)[*rule.occurrence]};
            if (!ymw.ok())
                return std::nullopt;
            return year_month_day{sys_days{ymw}};
        }
        case calendar_rule_kind::last_weekday_of_month: {
            if (!rule.month || !rule.weekday)
                return std::nullopt;
            const year_month_weekday_last ymwl{y, *rule.month, weekday_last{*rule.weekday}};
            if (!ymwl.ok())
                return std::nullopt;
            return year_month_day{sys_days{ymwl}};
        }
        case calendar_rule_kind::easter_offset: {
            if (!rule.day_offset)
                return std::nullopt;
            return year_month_day{sys_days{easter} + days{*rule.day_offset}};
        }
    }
    return std::nullopt;
}

std::chrono::year_month_day apply_shift(std::chrono::year_month_day date, observance_shift shift) {
    using namespace std::chrono;
    const sys_days d{date};
    const weekday w{d};

    switch (shift) {
        case observance_shift::none:
            return date;
        case observance_shift::nearest_weekday:
            if (w == Saturday)
                return year_month_day{d - days{1}};
            if (w == Sunday)
                return year_month_day{d + days{1}};
            return date;
        case observance_shift::roll_forward_to_monday:
            if (w == Saturday)
                return year_month_day{d + days{2}};
            if (w == Sunday)
                return year_month_day{d + days{1}};
            return date;
    }
    return date;
}

} // namespace

std::chrono::year_month_day calendar_rule_engine::easter_sunday(std::chrono::year y) {
    // Meeus/Jones/Butcher Gregorian Easter algorithm.
    const int yr = static_cast<int>(y);
    const int a = yr % 19;
    const int b = yr / 100;
    const int c = yr % 100;
    const int d = b / 4;
    const int e = b % 4;
    const int f = (b + 8) / 25;
    const int g = (b - f + 1) / 3;
    const int h = (19 * a + b - d - g + 15) % 30;
    const int i = c / 4;
    const int k = c % 4;
    const int l = (32 + 2 * e + 2 * i - h - k) % 7;
    const int m = (a + 11 * h + 22 * l) / 451;
    const int month = (h + l - 7 * m + 114) / 31;
    const int day = ((h + l - 7 * m + 114) % 31) + 1;

    return std::chrono::year_month_day{y,
                                       std::chrono::month{static_cast<unsigned>(month)},
                                       std::chrono::day{static_cast<unsigned>(day)}};
}

std::vector<instantiated_holiday>
calendar_rule_engine::instantiate_holidays_batch(std::span<const calendar_ruleset> calendars,
                                                 std::chrono::year_month_day start,
                                                 std::chrono::year_month_day end) {
    using namespace std::chrono;

    const sys_days start_days{start};
    const sys_days end_days{end};

    std::vector<std::set<year_month_day>> holidays_by_calendar(calendars.size());

    std::map<int, year_month_day> easter_cache;
    const auto easter_for = [&](year y) -> year_month_day {
        const int key = static_cast<int>(y);
        if (const auto it = easter_cache.find(key); it != easter_cache.end())
            return it->second;
        const auto e = easter_sunday(y);
        easter_cache.emplace(key, e);
        return e;
    };

    // Evaluate one extra year on each side: observance_shift (e.g.
    // nearest_weekday) can move a rule's observed date across a year
    // boundary -- e.g. a Saturday New Year's Day observes on the prior
    // Dec 31 -- so a rule whose *natural* date falls just outside
    // [start.year(), end.year()] can still land its *observed* date
    // inside [start, end]. The per-date filters below still clip the
    // final result to the requested window.
    for (year y = start.year() - years{1}; y <= end.year() + years{1}; ++y) {
        const auto easter = easter_for(y);

        for (std::size_t idx = 0; idx < calendars.size(); ++idx) {
            const auto& cal = calendars[idx];
            auto& holidays = holidays_by_calendar[idx];

            for (const auto& rule : cal.rules) {
                if (!rule.active_in(y))
                    continue;
                const auto natural = resolve_natural_date(rule, y, easter);
                if (!natural)
                    continue;
                const auto observed = apply_shift(*natural, rule.shift);
                const sys_days observed_days{observed};
                if (observed_days < start_days || observed_days > end_days)
                    continue;
                const weekday w{observed_days};
                if (is_weekend(w, cal.weekend_mask))
                    continue;
                holidays.insert(observed);
            }

            for (const auto& exc : cal.exceptions) {
                const sys_days exc_days{exc.date};
                if (exc_days < start_days || exc_days > end_days)
                    continue;
                if (exc.is_business_day)
                    holidays.erase(exc.date);
                else
                    holidays.insert(exc.date);
            }
        }
    }

    std::vector<instantiated_holiday> result;
    for (std::size_t idx = 0; idx < holidays_by_calendar.size(); ++idx) {
        for (const auto& date : holidays_by_calendar[idx])
            result.push_back(instantiated_holiday{idx, date});
    }
    return result;
}

} // namespace ores::analytics::quant::service
