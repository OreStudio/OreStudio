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
#include "ores.analytics.quant/service/fx_expiry_dates.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include <utility>

namespace ores::analytics::quant::service {

fx_expiry_date_rule::fx_expiry_date_rule(domain::business_day_convention roll,
                                         bool end_of_month,
                                         std::size_t calendar_index,
                                         domain::business_day_calendar_set calendars)
    : roll_(roll)
    , end_of_month_(end_of_month)
    , calendar_index_(calendar_index)
    , calendars_(std::move(calendars)) {}

std::chrono::year_month_day fx_expiry_date_rule::expiry(std::chrono::year_month_day spot_date,
                                                        domain::date_period period) const {
    return calendar_arithmetic::advance(
        spot_date, period, roll_, end_of_month_, calendar_index_, calendars_);
}

std::chrono::year_month_day fx_expiry_date_rule::business_day(std::chrono::year_month_day from,
                                                              int n) const {
    return expiry(from, {.unit = domain::period_unit::day, .multiplier = n});
}

}
