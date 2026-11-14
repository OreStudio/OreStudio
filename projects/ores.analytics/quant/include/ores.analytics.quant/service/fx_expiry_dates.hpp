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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_FX_EXPIRY_DATES_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_FX_EXPIRY_DATES_HPP

#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <string_view>

namespace ores::analytics::quant::service {

/**
 * @brief How one currency pair's FX dates are struck: its roll, month-end rule and calendars.
 *
 * The single way an FX option expiry gets a date. The forward ladder asks it for the date a
 * tenor settles on, and a volatility surface asks it for the date it strikes its quotes at, so
 * the two cannot disagree: both call the same rule, with the same convention, read from the
 * same refdata.
 *
 * The calendars are held by value. A rule is read once, when a feed or a ladder starts, and a
 * tick that reads it must not depend on a store that may have moved since.
 */
class ORES_ANALYTICS_QUANT_EXPORT fx_expiry_date_rule final {
public:
    /**
     * @brief The rule a caller holds before it has read the pair's convention: it dates nothing.
     *
     * @c calendar_count answers zero, and asking for a date throws @c std::out_of_range rather
     * than treating an unread calendar as one closed only at weekends. The value exists so that
     * a struct carrying a rule can be default built.
     */
    fx_expiry_date_rule() = default;

    /**
     * @brief The rule a pair's convention states.
     *
     * @param roll How a date that lands off a business day moves.
     * @param end_of_month Whether a month- or year-counted period snaps to the month's last
     * business day when the date it starts from is at or after that month's last one.
     * @param calendar_index Which of @p calendars is the pair's, whose holidays both currencies'
     * sets were joined into.
     * @param calendars The holiday set the rule rolls on.
     */
    fx_expiry_date_rule(domain::business_day_convention roll,
                        bool end_of_month,
                        std::size_t calendar_index,
                        domain::business_day_calendar_set calendars);

    /**
     * @brief The date @p period after @p spot_date, with the pair's convention and month-end
     * rule applied -- QuantLib's @c Calendar::advance.
     *
     * A day-counted period counts business days and ignores the convention, because the walk
     * cannot land on a holiday. A week-counted one is added as calendar time and adjusted. A
     * month- or year-counted one snaps to the month end when @p spot_date is at or after that
     * month's last business day and the pair's convention says it does.
     *
     * @throws std::invalid_argument if @p period states no unit.
     * @throws std::out_of_range if this rule's calendar index is not one of its calendars.
     */
    [[nodiscard]] std::chrono::year_month_day expiry(std::chrono::year_month_day spot_date,
                                                     domain::date_period period) const;

    /**
     * @brief The @p n th business day after @p from -- the walk a ladder uses for ON, TN and SN.
     *
     * @throws std::out_of_range if this rule's calendar index is not one of its calendars.
     */
    [[nodiscard]] std::chrono::year_month_day business_day(std::chrono::year_month_day from,
                                                           int n) const;

    /**
     * @brief The day a ladder counts from when its as-of is not a business day: @p as_of moved
     * under the pair's own convention.
     *
     * @throws std::out_of_range if this rule's calendar index is not one of its calendars.
     */
    [[nodiscard]] std::chrono::year_month_day horizon(std::chrono::year_month_day as_of) const;

    /**
     * @brief The day the pair settles on: @p spot_days business days after the horizon of
     * @p as_of.
     *
     * @throws std::out_of_range if this rule's calendar index is not one of its calendars.
     */
    [[nodiscard]] std::chrono::year_month_day spot_date(std::chrono::year_month_day as_of,
                                                        int spot_days) const;

    [[nodiscard]] domain::business_day_convention roll() const noexcept {
        return roll_;
    }

    [[nodiscard]] bool end_of_month() const noexcept {
        return end_of_month_;
    }

    /// How many calendars the rule was built with; zero means it dates nothing.
    [[nodiscard]] std::size_t calendar_count() const noexcept {
        return calendars_.calendar_count();
    }

private:
    domain::business_day_convention roll_{domain::business_day_convention::following};
    bool end_of_month_{false};
    std::size_t calendar_index_{0};
    domain::business_day_calendar_set calendars_;
};

/**
 * @brief The time a quoted tenor names, in years: the axis a volatility surface is quoted on.
 *
 * Days count over 365 and months over 12, so 1M is one twelfth of a year whatever the calendar
 * does to the date, 1W is seven 365ths and 1Y is one. This is the grid, not the settlement: the
 * date the cash moves is fx_expiry_date_rule's answer, and the two are deliberately different
 * questions. The generator annualises total variance over this and the calibration reads it back
 * over this, so producer and consumer cannot disagree about how long a tenor is.
 *
 * @return The year fraction, or nothing when @p tenor is not a count and a unit, or names no
 * time at all.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::optional<double>
fx_vol_tenor_years(std::string_view tenor);

}

#endif
