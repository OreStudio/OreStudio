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
#ifndef ORES_MARKETDATA_API_DOMAIN_TENOR_PERIOD_HPP
#define ORES_MARKETDATA_API_DOMAIN_TENOR_PERIOD_HPP

#include "ores.marketdata.api/export.hpp"
#include <chrono>
#include <compare>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The unit a tenor::count is expressed in.
 */
enum class tenor_period_unit { days, weeks, months, years };

/**
 * @brief A lightweight, QuantLib-free tenor period: a label identifying a point in
 * time relative to a horizon date (see
 * doc/knowledge/domain/tenor.org and doc/knowledge/domain/horizon_date.org).
 *
 * Deliberately does not model business-day calendars or holiday rolling —
 * that level of precision belongs to actual curve bootstrapping, which is
 * out of scope here (see doc/knowledge/domain/date_rolling_and_business_day_calendars.org).
 * The short-end labels (O/N, T/N, S/N, S/W) are approximated as fixed day
 * counts from the horizon date rather than resolved against real
 * spot-lag/holiday conventions.
 */
class ORES_MARKETDATA_API_EXPORT tenor_period final {
public:
    tenor_period() = default;
    tenor_period(tenor_period_unit unit, int count) : unit_(unit), count_(count) {}

    /**
     * @brief Parses a standard tenor label (O/N, T/N, S/N, S/W, or the
     * "<n><D|W|M|Y>" pattern, e.g. "1W", "3M", "2Y") into a tenor.
     *
     * @param label The tenor label to parse.
     * @throws std::invalid_argument if the label is not a recognised tenor.
     */
    static tenor_period parse(std::string_view label);

    tenor_period_unit unit() const { return unit_; }
    int count() const { return count_; }

    /**
     * @brief Approximate length in calendar days, used only to order and
     * compare tenors against one another (not for date arithmetic).
     */
    int approx_days() const;

    /**
     * @brief Resolves this tenor to an end date relative to a horizon date,
     * using std::chrono calendar arithmetic (no business-day adjustment).
     */
    std::chrono::year_month_day end_date(std::chrono::year_month_day horizon) const;

    std::strong_ordering operator<=>(const tenor_period& other) const {
        return approx_days() <=> other.approx_days();
    }
    bool operator==(const tenor_period& other) const = default;

private:
    tenor_period_unit unit_{tenor_period_unit::days};
    int count_{0};
};

/**
 * @brief A half-open date window [start, end) anchored to a horizon date and
 * a tenor.
 */
struct tenor_period_window final {
    std::chrono::year_month_day start;
    std::chrono::year_month_day end;
};

/**
 * @brief Resolves a (horizon, tenor) pair into its [start, end) date window,
 * where start is the horizon date itself.
 */
ORES_MARKETDATA_API_EXPORT tenor_period_window
resolve_window(std::chrono::year_month_day horizon, const tenor_period& t);

/**
 * @brief Whether two half-open date windows [a.start, a.end) and
 * [b.start, b.end) overlap.
 */
ORES_MARKETDATA_API_EXPORT bool windows_overlap(const tenor_period_window& a, const tenor_period_window& b);

}

#endif
