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
#include "conventions_family_pack.hpp"
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include <optional>
#include <sstream>
#include <string>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

void write_continuation_mappings(const std::string& s, record_writer mappings) {
    for (const auto& token : split_fields(s, ',')) {
        const auto colon = token.find(':');
        if (colon == std::string::npos)
            continue;
        auto m = mappings.append("ContinuationMapping");
        m.set_integer("From", std::stoll(token.substr(0, colon)));
        m.set_integer("To", std::stoll(token.substr(colon + 1)));
    }
}

std::string serialize_continuation_mappings(const std::optional<record_reader>& mappings) {
    std::string out;
    if (!mappings)
        return out;
    for (const auto& m : mappings->children("ContinuationMapping")) {
        if (!out.empty())
            out += ",";
        out += std::to_string(m.integer("From").value_or(0)) + ":" +
               std::to_string(m.integer("To").value_or(0));
    }
    return out;
}

}

refdata::domain::commodity_future_convention
conventions_mapper::map_commodity_future(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping commodity future convention: " << text_of(v, "Id");

    refdata::domain::commodity_future_convention r;
    r.id = text_of(v, "Id");
    r.contract_frequency = frequency_of(v, "ContractFrequency");
    r.calendar = text_of(v, "Calendar");
    assign(r.expiry_calendar, opt_text(v, "ExpiryCalendar"));
    assign(r.expiry_month_lag, opt_integer(v, "ExpiryMonthLag"));
    assign(r.one_contract_month, opt_spelling(v, "OneContractMonth"));
    assign(r.offset_days, opt_integer(v, "OffsetDays"));
    assign(r.business_day_convention, opt_bdc(v, "BusinessDayConvention"));
    assign(r.adjust_before_offset, opt_flag(v, "AdjustBeforeOffset"));
    assign(r.is_averaging, opt_flag(v, "IsAveraging"));

    if (const auto months = v.child("ValidContractMonths"))
        r.valid_contract_months = join_with(months->spellings("Month"), ',');

    if (const auto a = v.child("AnchorDay")) {
        if (const auto nth = a->child("NthWeekday")) {
            r.anchor_nth_nth = integer_of(*nth, "Nth");
            r.anchor_nth_weekday = opt_spelling(*nth, "Weekday");
        }
        assign(r.anchor_day_of_month, opt_integer(*a, "DayOfMonth"));
        assign(r.anchor_calendar_days_before, opt_integer(*a, "CalendarDaysBefore"));
        assign(r.anchor_last_weekday, opt_spelling(*a, "LastWeekday"));
        assign(r.anchor_weekly_day_of_the_week, opt_spelling(*a, "WeeklyDayOfTheWeek"));
        assign(r.anchor_business_days_after, opt_integer(*a, "BusinessDaysAfter"));
    }

    assign(r.option_expiry_month_lag, opt_integer(v, "OptionExpiryMonthLag"));
    assign(r.option_contract_frequency, opt_frequency(v, "OptionContractFrequency"));
    assign(r.option_expiry_offset, opt_integer(v, "OptionExpiryOffset"));
    assign(r.option_calendar_days_before, opt_integer(v, "OptionCalendarDaysBefore"));
    assign(r.option_min_business_days_before, opt_integer(v, "OptionMinBusinessDaysBefore"));
    assign(r.option_expiry_day, opt_integer(v, "OptionExpiryDay"));

    if (const auto nth = v.child("OptionNthWeekday")) {
        r.option_nth_nth = integer_of(*nth, "Nth");
        r.option_nth_weekday = opt_spelling(*nth, "Weekday");
    }

    assign(r.option_expiry_last_weekday_of_month,
           opt_spelling(v, "OptionExpiryLastWeekdayOfMonth"));
    assign(r.option_expiry_weekly_day_of_the_week,
           opt_spelling(v, "OptionExpiryWeeklyDayOfTheWeek"));
    assign(r.option_business_day_convention, opt_bdc(v, "OptionBusinessDayConvention"));
    assign(r.hours_per_day, opt_integer(v, "HoursPerDay"));

    if (const auto x = v.child("OffPeakPowerIndexData")) {
        r.off_peak_index = text_of(*x, "OffPeakIndex");
        r.peak_index = text_of(*x, "PeakIndex");
        r.off_peak_hours = decimal_of(*x, "OffPeakHours");
        r.peak_calendar = text_of(*x, "PeakCalendar");
    }

    assign(r.index_name, opt_text(v, "IndexName"));
    assign(r.savings_time, opt_text(v, "SavingsTime"));
    assign(r.delivery_location, opt_text(v, "DeliveryLocation"));
    assign(r.balance_of_the_month, opt_flag(v, "BalanceOfTheMonth"));
    assign(r.balance_of_the_month_pricing_calendar,
           opt_text(v, "BalanceOfTheMonthPricingCalendar"));
    assign(r.option_underlying_future_convention, opt_text(v, "OptionUnderlyingFutureConvention"));

    if (const auto a = v.child("AveragingData")) {
        r.averaging_commodity_name = text_of(*a, "CommodityName");
        r.averaging_period = spelling_of(*a, "Period", "averaging period");
        r.averaging_pricing_calendar = text_of(*a, "PricingCalendar");
        r.averaging_conventions = text_of(*a, "Conventions");
        assign(r.averaging_use_business_days, opt_flag(*a, "UseBusinessDays"));
        assign(r.averaging_delivery_roll_days, opt_integer(*a, "DeliveryRollDays"));
        assign(r.averaging_future_month_offset, opt_integer(*a, "FutureMonthOffset"));
        assign(r.averaging_daily_expiry_offset, opt_integer(*a, "DailyExpiryOffset"));
    }

    if (const auto prohibited = v.child("ProhibitedExpiries")) {
        std::string dates;
        if (const auto list = prohibited->child("Dates")) {
            for (const auto& d : list->children("Date")) {
                if (!dates.empty())
                    dates += ",";
                dates += d.own_text().value_or(std::string());
            }
        }
        r.prohibited_expiries = dates;
    }

    if (const auto mappings = v.child("FutureContinuationMappings"))
        r.future_continuation_mappings = serialize_continuation_mappings(mappings);

    if (const auto mappings = v.child("OptionContinuationMappings"))
        r.option_continuation_mappings = serialize_continuation_mappings(mappings);

    set_audit(r);
    return r;
}

void conventions_mapper::reverse_commodity_future(
    const refdata::domain::commodity_future_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    put_enum(w, "ContractFrequency", v.contract_frequency, "frequency type");
    w.set_text("Calendar", v.calendar);
    w.set_optional_text("ExpiryCalendar", v.expiry_calendar);
    w.set_optional_integer("ExpiryMonthLag", as_integer(v.expiry_month_lag));
    put_enum(w, "OneContractMonth", v.one_contract_month, "month");
    w.set_optional_integer("OffsetDays", as_integer(v.offset_days));
    put_bdc(w, "BusinessDayConvention", v.business_day_convention);
    w.set_optional_flag("AdjustBeforeOffset", v.adjust_before_offset);
    w.set_optional_flag("IsAveraging", v.is_averaging);
    if (v.valid_contract_months) {
        auto months = w.child("ValidContractMonths");
        std::istringstream in(*v.valid_contract_months);
        std::string token;
        while (std::getline(in, token, ',')) {
            if (!token.empty())
                months.append_spelling("Month", token, "month");
        }
    }
    if (v.anchor_nth_nth || v.anchor_day_of_month || v.anchor_calendar_days_before ||
        v.anchor_business_days_after || v.anchor_nth_weekday || v.anchor_last_weekday ||
        v.anchor_weekly_day_of_the_week) {
        auto a = w.child("AnchorDay");
        if (v.anchor_nth_nth || v.anchor_nth_weekday) {
            auto nth = a.child("NthWeekday");
            nth.set_integer("Nth", v.anchor_nth_nth.value_or(0));
            nth.set_spelling("Weekday", v.anchor_nth_weekday.value_or("Mon"), "weekday");
        }
        a.set_optional_integer("DayOfMonth", as_integer(v.anchor_day_of_month));
        a.set_optional_integer("CalendarDaysBefore", as_integer(v.anchor_calendar_days_before));
        put_enum(a, "LastWeekday", v.anchor_last_weekday, "weekday");
        put_enum(a, "WeeklyDayOfTheWeek", v.anchor_weekly_day_of_the_week, "weekday");
        a.set_optional_integer("BusinessDaysAfter", as_integer(v.anchor_business_days_after));
    }
    w.set_optional_integer("OptionExpiryMonthLag", as_integer(v.option_expiry_month_lag));
    put_enum(w, "OptionContractFrequency", v.option_contract_frequency, "frequency type");
    w.set_optional_integer("OptionExpiryOffset", as_integer(v.option_expiry_offset));
    w.set_optional_integer("OptionCalendarDaysBefore", as_integer(v.option_calendar_days_before));
    w.set_optional_integer("OptionMinBusinessDaysBefore",
                           as_integer(v.option_min_business_days_before));
    w.set_optional_integer("OptionExpiryDay", as_integer(v.option_expiry_day));
    if (v.option_nth_nth || v.option_nth_weekday) {
        auto nth = w.child("OptionNthWeekday");
        nth.set_integer("Nth", v.option_nth_nth.value_or(0));
        nth.set_spelling("Weekday", v.option_nth_weekday.value_or("Mon"), "weekday");
    }
    put_enum(w, "OptionExpiryLastWeekdayOfMonth", v.option_expiry_last_weekday_of_month, "weekday");
    put_enum(
        w, "OptionExpiryWeeklyDayOfTheWeek", v.option_expiry_weekly_day_of_the_week, "weekday");
    put_bdc(w, "OptionBusinessDayConvention", v.option_business_day_convention);
    w.set_optional_integer("HoursPerDay", as_integer(v.hours_per_day));
    if (v.off_peak_index && v.peak_index && v.off_peak_hours && v.peak_calendar) {
        auto off_peak = w.child("OffPeakPowerIndexData");
        off_peak.set_text("OffPeakIndex", *v.off_peak_index);
        off_peak.set_text("PeakIndex", *v.peak_index);
        put_decimal(off_peak, "OffPeakHours", *v.off_peak_hours);
        off_peak.set_text("PeakCalendar", *v.peak_calendar);
    }
    w.set_optional_text("IndexName", v.index_name);
    w.set_optional_text("SavingsTime", v.savings_time);
    w.set_optional_text("DeliveryLocation", v.delivery_location);
    w.set_optional_flag("BalanceOfTheMonth", v.balance_of_the_month);
    w.set_optional_text("BalanceOfTheMonthPricingCalendar",
                        v.balance_of_the_month_pricing_calendar);
    w.set_optional_text("OptionUnderlyingFutureConvention", v.option_underlying_future_convention);
    if (v.averaging_commodity_name && v.averaging_period && v.averaging_pricing_calendar &&
        v.averaging_conventions) {
        auto a = w.child("AveragingData");
        a.set_text("CommodityName", *v.averaging_commodity_name);
        a.set_spelling("Period", *v.averaging_period, "averaging period");
        a.set_text("PricingCalendar", *v.averaging_pricing_calendar);
        a.set_text("Conventions", *v.averaging_conventions);
        a.set_optional_flag("UseBusinessDays", v.averaging_use_business_days);
        a.set_optional_integer("DeliveryRollDays", as_integer(v.averaging_delivery_roll_days));
        a.set_optional_integer("FutureMonthOffset", as_integer(v.averaging_future_month_offset));
        a.set_optional_integer("DailyExpiryOffset", as_integer(v.averaging_daily_expiry_offset));
    }
    if (v.prohibited_expiries) {
        auto dates = w.child("ProhibitedExpiries").child("Dates");
        std::istringstream in(*v.prohibited_expiries);
        std::string token;
        while (std::getline(in, token, ',')) {
            if (token.empty())
                continue;
            auto d = dates.append("Date");
            d.set_own_text(token);
        }
    }
    if (v.future_continuation_mappings)
        write_continuation_mappings(*v.future_continuation_mappings,
                                    w.child("FutureContinuationMappings"));
    if (v.option_continuation_mappings)
        write_continuation_mappings(*v.option_continuation_mappings,
                                    w.child("OptionContinuationMappings"));
}

namespace conventions_family {

void pack_commodity_future(const record_reader& root,
                           refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "commodity_future",
                root.children("CommodityFuture"),
                &conventions_mapper::map_commodity_future);
}

void join_commodity_future(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_commodity_future(
        unpack_row<refdata::domain::commodity_future_convention>(row),
        root.append("CommodityFuture"));
}

}

}
