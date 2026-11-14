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

    /// The calendars the rule rolls on, for the few callers that ask a date question the rule
    /// does not name -- such as where a ladder starts when the as-of is not a business day.
    [[nodiscard]] const domain::business_day_calendar_set& calendars() const noexcept {
        return calendars_;
    }

private:
    domain::business_day_convention roll_{domain::business_day_convention::following};
    bool end_of_month_{false};
    std::size_t calendar_index_{0};
    domain::business_day_calendar_set calendars_;
};

}

#endif
