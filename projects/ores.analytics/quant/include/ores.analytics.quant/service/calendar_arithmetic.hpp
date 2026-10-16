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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_CALENDAR_ARITHMETIC_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_CALENDAR_ARITHMETIC_HPP

#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <cstddef>

namespace ores::analytics::quant::service {

/**
 * @brief The date arithmetic QuantLib's @c Date and @c Calendar define, as
 * pure functions over a date and a calendar set.
 *
 * Every function transcribes one QuantLib entry point and keeps that entry
 * point's name where the name is not ambiguous here:
 *
 * | this | QuantLib |
 * |------|----------|
 * | @c add_period | @c Date::advance(Date, Integer, TimeUnit) |
 * | @c adjust | @c Calendar::adjust(Date, BusinessDayConvention) |
 * | @c advance | @c Calendar::advance(Date, Integer, TimeUnit, BDC, bool, optional<BDC>) |
 * | @c is_end_of_month | @c Calendar::isEndOfMonth |
 * | @c last_business_day | @c Calendar::endOfMonth |
 * | @c nth_weekday | @c Date::nthWeekday |
 * | @c imm_date, @c is_imm_date, @c next_imm_date | @c IMM::date, @c isIMMdate, @c nextDate |
 * | @c asx_date, @c is_asx_date, @c next_asx_date | @c ASX::date, @c isASXdate, @c nextDate |
 *
 * Two of QuantLib's behaviours are counter-intuitive enough to state here,
 * because a reader who assumes otherwise will get dates wrong:
 *
 * - @c add_period counts *calendar* days for a day-counted period, while
 *   @c advance counts *business* days for the same period and ignores the
 *   convention it was given. They are different functions on purpose.
 * - @c advance with @c end_of_month set and a month- or year-counted period
 *   ignores the convention entirely and snaps to the end of the month
 *   adjusted by @c business_day_convention::preceding.
 *
 * Every function here requires @p calendar_index to name a calendar the set
 * was built with. A caller at a system boundary validates that first and
 * reports it; these are the primitives underneath that boundary and throw
 * @c std::out_of_range rather than silently treating an unknown calendar as
 * one with no holidays.
 */
class ORES_ANALYTICS_QUANT_EXPORT calendar_arithmetic final {
public:
    /**
     * @brief The date @p period of calendar time after @p date -- QuantLib's
     * @c Date::advance.
     *
     * A month- or year-counted period that lands on a day the target month
     * does not have clamps to its last day, once, against the total: 31
     * January plus two months is 31 March, not 28 March. No calendar is
     * consulted and the result may be a weekend.
     *
     * @throws std::invalid_argument if @p period states no unit.
     */
    [[nodiscard]] static std::chrono::year_month_day add_period(std::chrono::year_month_day date,
                                                                domain::date_period period);

    /**
     * @brief The first date at or after @p date that @p convention accepts --
     * QuantLib's @c Calendar::adjust.
     *
     * @c business_day_convention::unadjusted returns @p date untouched.
     *
     * @param date The date to move, if it needs moving.
     * @param convention How far it may move, and which way.
     * @param calendar_index Which of @p calendars' holiday sets to roll on.
     * @param calendars The sets every calendar index refers to.
     *
     * @throws std::out_of_range if @p calendar_index is not one of
     * @p calendars.
     */
    [[nodiscard]] static std::chrono::year_month_day
    adjust(std::chrono::year_month_day date,
           domain::business_day_convention convention,
           std::size_t calendar_index,
           const domain::business_day_calendar_set& calendars);

    /**
     * @brief The date @p period after @p date, with @p convention applied --
     * QuantLib's @c Calendar::advance.
     *
     * For a day-counted period the walk counts business days and
     * @p convention is unused, because the walk cannot land on a holiday. For
     * a week-counted period @p period is added as calendar time and then
     * adjusted. For a month- or year-counted period, @p end_of_month snaps
     * the result to the month's last business day whenever @p date is itself
     * at or after that month's last business day.
     *
     * @param date The date to count from.
     * @param period How far to count. A zero multiplier still adjusts
     * @p date, which is what QuantLib does.
     * @param convention How a date that lands off a business day moves.
     * @param end_of_month Snap a month- or year-counted period to the end of
     * the target month.
     * @param calendar_index Which of @p calendars' holiday sets to roll on.
     * @param calendars The sets every calendar index refers to.
     *
     * @throws std::invalid_argument if @p period states no unit.
     * @throws std::out_of_range if @p calendar_index is not one of
     * @p calendars.
     */
    [[nodiscard]] static std::chrono::year_month_day
    advance(std::chrono::year_month_day date,
            domain::date_period period,
            domain::business_day_convention convention,
            bool end_of_month,
            std::size_t calendar_index,
            const domain::business_day_calendar_set& calendars);

    /**
     * @brief Whether @p date is at or after the last business day of its
     * month -- QuantLib's @c Calendar::isEndOfMonth, the predicate its
     * @c advance uses to decide whether to snap a period to the month end.
     *
     * "At or after", not "is": a Saturday 31st answers true, because no
     * business day follows it in that month, even though the 31st is not
     * itself a business day. That is a different question again from whether
     * @p date is the month's last calendar day.
     *
     * @throws std::out_of_range if @p calendar_index is not one of
     * @p calendars.
     */
    [[nodiscard]] static bool is_end_of_month(std::chrono::year_month_day date,
                                              std::size_t calendar_index,
                                              const domain::business_day_calendar_set& calendars);

    /**
     * @brief The last business day of @p date's month, moved by @p convention
     * -- QuantLib's @c Calendar::endOfMonth, whose default is @c preceding.
     *
     * @throws std::out_of_range if @p calendar_index is not one of
     * @p calendars.
     */
    [[nodiscard]] static std::chrono::year_month_day
    last_business_day(std::chrono::year_month_day date,
                      domain::business_day_convention convention,
                      std::size_t calendar_index,
                      const domain::business_day_calendar_set& calendars);

    /**
     * @brief The @p nth @p weekday of @p month @p year, counting from one --
     * QuantLib's @c Date::nthWeekday.
     *
     * @throws std::invalid_argument if @p nth is zero or greater than five.
     */
    [[nodiscard]] static std::chrono::year_month_day nth_weekday(unsigned nth,
                                                                 std::chrono::weekday weekday,
                                                                 std::chrono::month month,
                                                                 std::chrono::year year);

    /**
     * @brief The IMM date for @p month @p year: the third Wednesday --
     * QuantLib's @c IMM::date.
     *
     * Used by exchange-traded contracts, and not the same date as the CDS
     * roll date (doc/knowledge/domain/imm_dates.org).
     */
    [[nodiscard]] static std::chrono::year_month_day imm_date(std::chrono::month month,
                                                              std::chrono::year year);

    /**
     * @brief Whether @p date is a third-Wednesday IMM date -- QuantLib's
     * @c IMM::isIMMdate.
     *
     * @param date The date to test.
     * @param main_cycle When true, only the March, June, September and
     * December ones count.
     */
    [[nodiscard]] static bool is_imm_date(std::chrono::year_month_day date, bool main_cycle = true);

    /**
     * @brief The first IMM date strictly after @p date -- QuantLib's
     * @c IMM::nextDate.
     *
     * @param main_cycle When true, only the March, June, September and
     * December dates are considered.
     */
    [[nodiscard]] static std::chrono::year_month_day next_imm_date(std::chrono::year_month_day date,
                                                                   bool main_cycle = true);

    /**
     * @brief The ASX date for @p month @p year: the second Friday --
     * QuantLib's @c ASX::date.
     *
     * The Australian exchange's contract date, and a different weekday from
     * the IMM date's third Wednesday.
     */
    [[nodiscard]] static std::chrono::year_month_day asx_date(std::chrono::month month,
                                                              std::chrono::year year);

    /**
     * @brief Whether @p date is a second-Friday ASX date -- QuantLib's
     * @c ASX::isASXdate.
     *
     * @param date The date to test.
     * @param main_cycle When true, only the March, June, September and
     * December ones count.
     */
    [[nodiscard]] static bool is_asx_date(std::chrono::year_month_day date, bool main_cycle = true);

    /**
     * @brief The first ASX date strictly after @p date -- QuantLib's
     * @c ASX::nextDate.
     *
     * @param main_cycle When true, only the March, June, September and
     * December dates are considered.
     */
    [[nodiscard]] static std::chrono::year_month_day next_asx_date(std::chrono::year_month_day date,
                                                                   bool main_cycle = true);
};

} // namespace ores::analytics::quant::service

#endif
