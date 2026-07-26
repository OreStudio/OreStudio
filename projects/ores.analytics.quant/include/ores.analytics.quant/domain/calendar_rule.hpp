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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_CALENDAR_RULE_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_CALENDAR_RULE_HPP

#include <chrono>
#include <optional>

namespace ores::analytics::quant::domain {

/// The four primitives real-world calendars decompose into (transcribed from
/// QuantLib's own calendar sources -- see the task plan for the survey of
/// convergent independent implementations this mirrors).
enum class calendar_rule_kind {
    /// A specific day/month every year, e.g. Christmas (25 December).
    fixed_date,
    /// The nth occurrence of a weekday in a month, e.g. the 4th Thursday of
    /// November (US Thanksgiving).
    nth_weekday_of_month,
    /// The last occurrence of a weekday in a month, e.g. the last Monday of
    /// August (UK Summer Bank Holiday).
    last_weekday_of_month,
    /// A fixed offset (in days) from Easter Sunday, e.g. Good Friday
    /// (offset -2) or Easter Monday (offset +1).
    easter_offset,
};

/// How a holiday that falls on a weekend is observed, if at all. Applied
/// after the rule itself resolves to a candidate date.
enum class observance_shift {
    /// No substitute observance -- the holiday simply falls on the weekend
    /// with no effect (this is the common case: most rules already only
    /// matter on business days, or the calendar doesn't roll at all).
    none,
    /// Saturday moves to the preceding Friday, Sunday to the following
    /// Monday (the classic US federal-holiday convention).
    nearest_weekday,
    /// Saturday or Sunday both move to the following Monday (the UK
    /// convention, e.g. New Year's Day).
    roll_forward_to_monday,
};

/// One indefinite, timeless recurring rule for a calendar -- "what day this
/// holiday falls on in any given year", not a materialised date. Only
/// meaningful for base-less calendars in the wider refdata model; this
/// library knows nothing of calendars, codes, or persistence, only the rule
/// shape itself (see this component's no-refdata-coupling convention).
struct calendar_rule final {
    calendar_rule_kind kind;
    std::optional<std::chrono::month> month;
    std::optional<unsigned> day;                 // fixed_date
    std::optional<std::chrono::weekday> weekday;  // nth/last_weekday_of_month
    std::optional<unsigned> occurrence;           // nth_weekday_of_month: 1..4
    std::optional<int> day_offset;                // easter_offset (e.g. -2 = Good Friday)
    observance_shift shift = observance_shift::none;
    std::optional<std::chrono::year> effective_from;
    std::optional<std::chrono::year> effective_to;

    [[nodiscard]] bool active_in(std::chrono::year y) const noexcept {
        if (effective_from && y < *effective_from)
            return false;
        if (effective_to && y > *effective_to)
            return false;
        return true;
    }
};

/// A one-off override for a single calendar day -- QuantLib's "special
/// closings"/Jubilee-style irregular dates, kept as flat data rather than
/// folded into the rule function they were transcribed from.
struct calendar_exception final {
    std::chrono::year_month_day date;
    /// true = override to open (a business day despite falling on what a
    /// rule would otherwise mark a holiday); false = additional holiday.
    bool is_business_day;
};

} // namespace ores::analytics::quant::domain

#endif
