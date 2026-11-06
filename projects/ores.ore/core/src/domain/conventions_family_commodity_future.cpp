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
#include "ores.utility/codec/json_codec.hpp"
#include "conventions_family_pack.hpp"
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <format>
#include <map>
#include <optional>
#include <rfl/json.hpp>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

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

namespace {

/// Whether the element states any field of the option expiry part.
bool states_option_expiry(const refdata::domain::commodity_future_option_expiry& o) {
    return o.option_expiry_month_lag || o.option_contract_frequency || o.option_expiry_offset ||
           o.option_calendar_days_before || o.option_min_business_days_before ||
           o.option_expiry_day || o.option_nth_nth || o.option_nth_weekday ||
           o.option_expiry_last_weekday_of_month || o.option_expiry_weekly_day_of_the_week ||
           o.option_business_day_convention || o.option_underlying_future_convention ||
           o.option_continuation_mappings;
}

/// The one row of an optional part, or null when the convention has none. A
/// convention holds at most one row of each optional part.
template <typename Part>
const Part* at_most_one(const std::vector<Part>& parts, std::string_view convention) {
    if (parts.size() > 1)
        throw std::runtime_error(std::format(
            "Commodity future convention \"{}\" has more than one row of an optional part.",
            convention));
    return parts.empty() ? nullptr : &parts.front();
}

}

ores::refdata::messaging::commodity_future_convention_item
conventions_mapper::map_commodity_future(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping commodity future convention: " << text_of(v, "Id");

    ores::refdata::messaging::commodity_future_convention_item r;
    refdata::domain::convention central;
    central.identity.kind_code = std::string(conventions_family::commodity_future_kind);
    central.payload.name = text_of(v, "Id");
    central.identity.id = provisional_uuid();
    set_audit(central);
    r.key.id = central.identity.id;
    r.convention = central;

    refdata::domain::commodity_future_contract_terms terms;
    terms.id = provisional_uuid();
    terms.convention_id = central.identity.id;
    terms.contract_frequency = frequency_of(v, "ContractFrequency");
    terms.calendar = text_of(v, "Calendar");
    assign(terms.expiry_calendar, opt_text(v, "ExpiryCalendar"));
    assign(terms.expiry_month_lag, opt_integer(v, "ExpiryMonthLag"));
    assign(terms.one_contract_month, opt_spelling(v, "OneContractMonth"));
    assign(terms.offset_days, opt_integer(v, "OffsetDays"));
    assign(terms.business_day_convention, opt_bdc(v, "BusinessDayConvention"));
    assign(terms.adjust_before_offset, opt_flag(v, "AdjustBeforeOffset"));
    assign(terms.is_averaging, opt_flag(v, "IsAveraging"));
    if (const auto months = v.child("ValidContractMonths"))
        terms.valid_contract_months = join_with(months->spellings("Month"), ',');
    assign(terms.hours_per_day, opt_integer(v, "HoursPerDay"));
    assign(terms.index_name, opt_text(v, "IndexName"));
    assign(terms.savings_time, opt_text(v, "SavingsTime"));
    assign(terms.delivery_location, opt_text(v, "DeliveryLocation"));
    assign(terms.balance_of_the_month, opt_flag(v, "BalanceOfTheMonth"));
    assign(terms.balance_of_the_month_pricing_calendar,
           opt_text(v, "BalanceOfTheMonthPricingCalendar"));
    if (const auto prohibited = v.child("ProhibitedExpiries")) {
        std::string dates;
        if (const auto list = prohibited->child("Dates")) {
            for (const auto& d : list->children("Date")) {
                if (!dates.empty())
                    dates += ",";
                dates += d.own_text().value_or(std::string());
            }
        }
        terms.prohibited_expiries = dates;
    }
    if (const auto mappings = v.child("FutureContinuationMappings"))
        terms.future_continuation_mappings = serialize_continuation_mappings(mappings);
    set_audit(terms);
    r.contract_terms.push_back(std::move(terms));

    if (const auto a = v.child("AnchorDay")) {
        refdata::domain::commodity_future_expiry_anchor anchor;
        anchor.id = provisional_uuid();
        anchor.convention_id = central.identity.id;
        if (const auto nth = a->child("NthWeekday")) {
            anchor.anchor_nth_nth = integer_of(*nth, "Nth");
            anchor.anchor_nth_weekday = opt_spelling(*nth, "Weekday");
        }
        assign(anchor.anchor_day_of_month, opt_integer(*a, "DayOfMonth"));
        assign(anchor.anchor_calendar_days_before, opt_integer(*a, "CalendarDaysBefore"));
        assign(anchor.anchor_last_weekday, opt_spelling(*a, "LastWeekday"));
        assign(anchor.anchor_weekly_day_of_the_week, opt_spelling(*a, "WeeklyDayOfTheWeek"));
        assign(anchor.anchor_business_days_after, opt_integer(*a, "BusinessDaysAfter"));
        set_audit(anchor);
        r.expiry_anchor.push_back(std::move(anchor));
    }

    refdata::domain::commodity_future_option_expiry option;
    assign(option.option_expiry_month_lag, opt_integer(v, "OptionExpiryMonthLag"));
    assign(option.option_contract_frequency, opt_frequency(v, "OptionContractFrequency"));
    assign(option.option_expiry_offset, opt_integer(v, "OptionExpiryOffset"));
    assign(option.option_calendar_days_before, opt_integer(v, "OptionCalendarDaysBefore"));
    assign(option.option_min_business_days_before, opt_integer(v, "OptionMinBusinessDaysBefore"));
    assign(option.option_expiry_day, opt_integer(v, "OptionExpiryDay"));
    if (const auto nth = v.child("OptionNthWeekday")) {
        option.option_nth_nth = integer_of(*nth, "Nth");
        option.option_nth_weekday = opt_spelling(*nth, "Weekday");
    }
    assign(option.option_expiry_last_weekday_of_month,
           opt_spelling(v, "OptionExpiryLastWeekdayOfMonth"));
    assign(option.option_expiry_weekly_day_of_the_week,
           opt_spelling(v, "OptionExpiryWeeklyDayOfTheWeek"));
    assign(option.option_business_day_convention, opt_bdc(v, "OptionBusinessDayConvention"));
    assign(option.option_underlying_future_convention,
           opt_text(v, "OptionUnderlyingFutureConvention"));
    if (const auto mappings = v.child("OptionContinuationMappings"))
        option.option_continuation_mappings = serialize_continuation_mappings(mappings);
    if (states_option_expiry(option)) {
        option.id = provisional_uuid();
        option.convention_id = central.identity.id;
        set_audit(option);
        r.option_expiry.push_back(std::move(option));
    }

    if (const auto x = v.child("OffPeakPowerIndexData")) {
        refdata::domain::commodity_future_off_peak_index off_peak;
        off_peak.id = provisional_uuid();
        off_peak.convention_id = central.identity.id;
        off_peak.off_peak_index = text_of(*x, "OffPeakIndex");
        off_peak.peak_index = text_of(*x, "PeakIndex");
        off_peak.off_peak_hours = decimal_of(*x, "OffPeakHours");
        off_peak.peak_calendar = text_of(*x, "PeakCalendar");
        set_audit(off_peak);
        r.off_peak_index.push_back(std::move(off_peak));
    }

    if (const auto a = v.child("AveragingData")) {
        refdata::domain::commodity_future_averaging_terms averaging;
        averaging.id = provisional_uuid();
        averaging.convention_id = central.identity.id;
        averaging.averaging_commodity_name = text_of(*a, "CommodityName");
        averaging.averaging_period = spelling_of(*a, "Period", "averaging period");
        averaging.averaging_pricing_calendar = text_of(*a, "PricingCalendar");
        averaging.averaging_conventions = text_of(*a, "Conventions");
        assign(averaging.averaging_use_business_days, opt_flag(*a, "UseBusinessDays"));
        assign(averaging.averaging_delivery_roll_days, opt_integer(*a, "DeliveryRollDays"));
        assign(averaging.averaging_future_month_offset, opt_integer(*a, "FutureMonthOffset"));
        assign(averaging.averaging_daily_expiry_offset, opt_integer(*a, "DailyExpiryOffset"));
        set_audit(averaging);
        r.averaging_terms.push_back(std::move(averaging));
    }
    return r;
}

// The schema fixes the order of the element's children, and it interleaves the
// parts: the anchor day sits among the contract terms, and the option
// underlying comes after the power and balance of the month fields.
void conventions_mapper::reverse_commodity_future(
    const ores::refdata::messaging::commodity_future_convention_item& v, record_writer w) {
    if (!v.convention || v.contract_terms.size() != 1)
        throw std::runtime_error(
            std::format("Commodity future convention \"{}\" needs its central row and one "
                        "contract terms row.",
                        v.convention ? v.convention->payload.name : std::string("?")));

    const auto& name = v.convention->payload.name;
    const auto& t = v.contract_terms.front();
    const auto* anchor = at_most_one(v.expiry_anchor, name);
    const auto* option = at_most_one(v.option_expiry, name);
    const auto* off_peak = at_most_one(v.off_peak_index, name);
    const auto* averaging = at_most_one(v.averaging_terms, name);

    w.set_text("Id", v.convention->payload.name);
    put_enum(w, "ContractFrequency", t.contract_frequency, "frequency type");
    w.set_text("Calendar", t.calendar);
    w.set_optional_text("ExpiryCalendar", t.expiry_calendar);
    w.set_optional_integer("ExpiryMonthLag", as_integer(t.expiry_month_lag));
    put_enum(w, "OneContractMonth", t.one_contract_month, "month");
    w.set_optional_integer("OffsetDays", as_integer(t.offset_days));
    put_bdc(w, "BusinessDayConvention", t.business_day_convention);
    w.set_optional_flag("AdjustBeforeOffset", t.adjust_before_offset);
    w.set_optional_flag("IsAveraging", t.is_averaging);
    if (t.valid_contract_months) {
        auto months = w.child("ValidContractMonths");
        std::istringstream in(*t.valid_contract_months);
        std::string token;
        while (std::getline(in, token, ',')) {
            if (!token.empty())
                months.append_spelling("Month", token, "month");
        }
    }
    if (anchor && (anchor->anchor_nth_nth || anchor->anchor_day_of_month ||
                   anchor->anchor_calendar_days_before || anchor->anchor_business_days_after ||
                   anchor->anchor_nth_weekday || anchor->anchor_last_weekday ||
                   anchor->anchor_weekly_day_of_the_week)) {
        auto a = w.child("AnchorDay");
        if (anchor->anchor_nth_nth || anchor->anchor_nth_weekday) {
            auto nth = a.child("NthWeekday");
            nth.set_integer("Nth", anchor->anchor_nth_nth.value_or(0));
            nth.set_spelling("Weekday", anchor->anchor_nth_weekday.value_or("Mon"), "weekday");
        }
        a.set_optional_integer("DayOfMonth", as_integer(anchor->anchor_day_of_month));
        a.set_optional_integer("CalendarDaysBefore",
                               as_integer(anchor->anchor_calendar_days_before));
        put_enum(a, "LastWeekday", anchor->anchor_last_weekday, "weekday");
        put_enum(a, "WeeklyDayOfTheWeek", anchor->anchor_weekly_day_of_the_week, "weekday");
        a.set_optional_integer("BusinessDaysAfter", as_integer(anchor->anchor_business_days_after));
    }
    if (option) {
        w.set_optional_integer("OptionExpiryMonthLag", as_integer(option->option_expiry_month_lag));
        put_enum(w, "OptionContractFrequency", option->option_contract_frequency, "frequency type");
        w.set_optional_integer("OptionExpiryOffset", as_integer(option->option_expiry_offset));
        w.set_optional_integer("OptionCalendarDaysBefore",
                               as_integer(option->option_calendar_days_before));
        w.set_optional_integer("OptionMinBusinessDaysBefore",
                               as_integer(option->option_min_business_days_before));
        w.set_optional_integer("OptionExpiryDay", as_integer(option->option_expiry_day));
        if (option->option_nth_nth || option->option_nth_weekday) {
            auto nth = w.child("OptionNthWeekday");
            nth.set_integer("Nth", option->option_nth_nth.value_or(0));
            nth.set_spelling("Weekday", option->option_nth_weekday.value_or("Mon"), "weekday");
        }
        put_enum(w,
                 "OptionExpiryLastWeekdayOfMonth",
                 option->option_expiry_last_weekday_of_month,
                 "weekday");
        put_enum(w,
                 "OptionExpiryWeeklyDayOfTheWeek",
                 option->option_expiry_weekly_day_of_the_week,
                 "weekday");
        put_bdc(w, "OptionBusinessDayConvention", option->option_business_day_convention);
    }
    w.set_optional_integer("HoursPerDay", as_integer(t.hours_per_day));
    if (off_peak) {
        auto x = w.child("OffPeakPowerIndexData");
        x.set_text("OffPeakIndex", off_peak->off_peak_index);
        x.set_text("PeakIndex", off_peak->peak_index);
        put_decimal(x, "OffPeakHours", off_peak->off_peak_hours);
        x.set_text("PeakCalendar", off_peak->peak_calendar);
    }
    w.set_optional_text("IndexName", t.index_name);
    w.set_optional_text("SavingsTime", t.savings_time);
    w.set_optional_text("DeliveryLocation", t.delivery_location);
    w.set_optional_flag("BalanceOfTheMonth", t.balance_of_the_month);
    w.set_optional_text("BalanceOfTheMonthPricingCalendar",
                        t.balance_of_the_month_pricing_calendar);
    if (option)
        w.set_optional_text("OptionUnderlyingFutureConvention",
                            option->option_underlying_future_convention);
    if (averaging) {
        auto a = w.child("AveragingData");
        a.set_text("CommodityName", averaging->averaging_commodity_name);
        a.set_spelling("Period", averaging->averaging_period, "averaging period");
        a.set_text("PricingCalendar", averaging->averaging_pricing_calendar);
        a.set_text("Conventions", averaging->averaging_conventions);
        a.set_optional_flag("UseBusinessDays", averaging->averaging_use_business_days);
        a.set_optional_integer("DeliveryRollDays",
                               as_integer(averaging->averaging_delivery_roll_days));
        a.set_optional_integer("FutureMonthOffset",
                               as_integer(averaging->averaging_future_month_offset));
        a.set_optional_integer("DailyExpiryOffset",
                               as_integer(averaging->averaging_daily_expiry_offset));
    }
    if (t.prohibited_expiries) {
        auto dates = w.child("ProhibitedExpiries").child("Dates");
        std::istringstream in(*t.prohibited_expiries);
        std::string token;
        while (std::getline(in, token, ',')) {
            if (token.empty())
                continue;
            auto d = dates.append("Date");
            d.set_own_text(token);
        }
    }
    if (t.future_continuation_mappings)
        write_continuation_mappings(*t.future_continuation_mappings,
                                    w.child("FutureContinuationMappings"));
    if (option && option->option_continuation_mappings)
        write_continuation_mappings(*option->option_continuation_mappings,
                                    w.child("OptionContinuationMappings"));
}

namespace conventions_family {

void pack_commodity_future(const record_reader& root,
                           refdata::messaging::conventions_bundle& bundle) {
    int conventions_seen = 0;
    int averaging_seen = 0;
    int terms_seen = 0;
    int anchor_seen = 0;
    int off_peak_seen = 0;
    int option_seen = 0;
    for (const auto& x : root.children("CommodityFuture")) {
        auto item = conventions_mapper::map_commodity_future(x);
        if (item.convention)
            pack_row(bundle, "convention", conventions_seen++, *item.convention);
        for (const auto& part : item.averaging_terms)
            pack_row(bundle, "commodity_future_averaging_terms", averaging_seen++, part);
        for (const auto& part : item.contract_terms)
            pack_row(bundle, "commodity_future_contract_terms", terms_seen++, part);
        for (const auto& part : item.expiry_anchor)
            pack_row(bundle, "commodity_future_expiry_anchor", anchor_seen++, part);
        for (const auto& part : item.off_peak_index)
            pack_row(bundle, "commodity_future_off_peak_index", off_peak_seen++, part);
        for (const auto& part : item.option_expiry)
            pack_row(bundle, "commodity_future_option_expiry", option_seen++, part);
    }
}

namespace {

/// The parts of every commodity future the rows hold, one vector per part.
struct commodity_future_parts {
    std::vector<refdata::domain::commodity_future_averaging_terms> averaging;
    std::vector<refdata::domain::commodity_future_contract_terms> terms;
    std::vector<refdata::domain::commodity_future_expiry_anchor> anchor;
    std::vector<refdata::domain::commodity_future_off_peak_index> off_peak;
    std::vector<refdata::domain::commodity_future_option_expiry> option;

    /// Keeps a part row, or says the family is not a part of this kind.
    bool keep(const package_row& row) {
        if (row.family == "commodity_future_averaging_terms")
            averaging.push_back(unpack_row<refdata::domain::commodity_future_averaging_terms>(row));
        else if (row.family == "commodity_future_contract_terms")
            terms.push_back(unpack_row<refdata::domain::commodity_future_contract_terms>(row));
        else if (row.family == "commodity_future_expiry_anchor")
            anchor.push_back(unpack_row<refdata::domain::commodity_future_expiry_anchor>(row));
        else if (row.family == "commodity_future_off_peak_index")
            off_peak.push_back(unpack_row<refdata::domain::commodity_future_off_peak_index>(row));
        else if (row.family == "commodity_future_option_expiry")
            option.push_back(unpack_row<refdata::domain::commodity_future_option_expiry>(row));
        else
            return false;
        return true;
    }

    /// The family of the first part that names none of the conventions, if any.
    std::optional<std::string> orphan_of(const std::set<boost::uuids::uuid>& ids) const {
        const auto named = [&](const auto& rows) {
            return std::ranges::all_of(
                rows, [&](const auto& r) { return ids.contains(r.convention_id); });
        };
        if (!named(averaging))
            return "commodity_future_averaging_terms";
        if (!named(terms))
            return "commodity_future_contract_terms";
        if (!named(anchor))
            return "commodity_future_expiry_anchor";
        if (!named(off_peak))
            return "commodity_future_off_peak_index";
        if (!named(option))
            return "commodity_future_option_expiry";
        return std::nullopt;
    }

    /// The item of one central row, with the parts that name it. The transform
    /// is applied to each part, so the facts can clear the keys.
    template <typename Transform>
    ores::refdata::messaging::commodity_future_convention_item
    item_of(const refdata::domain::convention& c, Transform transform) const {
        ores::refdata::messaging::commodity_future_convention_item item;
        item.key.id = c.identity.id;
        item.convention = c;
        const auto take = [&](const auto& from, auto& to) {
            for (const auto& part : from)
                if (part.convention_id == c.identity.id)
                    to.push_back(transform(part));
        };
        take(averaging, item.averaging_terms);
        take(terms, item.contract_terms);
        take(anchor, item.expiry_anchor);
        take(off_peak, item.off_peak_index);
        take(option, item.option_expiry);
        return item;
    }
};

}

void join_commodity_future(const std::vector<const package_row*>& rows, record_writer& root) {
    std::vector<refdata::domain::convention> central;
    commodity_future_parts parts;

    for (const auto* row : rows) {
        // The central rows of every kind reach this join, and only this kind's
        // rows are a commodity future element.
        if (row->family == "convention") {
            auto c = unpack_row<refdata::domain::convention>(*row);
            if (c.identity.kind_code == commodity_future_kind)
                central.push_back(std::move(c));
        } else if (!parts.keep(*row)) {
            throw std::runtime_error(
                std::format("Family \"{}\" is not a commodity future family.", row->family));
        }
    }

    // A part names its convention by id, so a part whose convention is not here
    // would be lost from the export rather than written.
    std::set<boost::uuids::uuid> ids;
    for (const auto& c : central)
        ids.insert(c.identity.id);
    if (const auto orphan = parts.orphan_of(ids))
        throw std::runtime_error(std::format(
            "Commodity future part \"{}\" names no commodity future convention.", *orphan));

    const auto as_is = [](const auto& part) {
        return part;
    };
    for (const auto& c : central)
        conventions_mapper::reverse_commodity_future(parts.item_of(c, as_is),
                                                     root.append("CommodityFuture"));
}

/// A commodity future's facts, keyed by the identity the mapper states, for the
/// reason the tenor basis swap's are.
std::map<std::string, std::string>
commodity_future_facts(const refdata::messaging::conventions_bundle& bundle) {
    commodity_future_parts parts;
    for (const auto& row : bundle.rows)
        parts.keep(row);

    const auto without_keys = [](auto part) {
        part.id = boost::uuids::uuid{};
        part.convention_id = boost::uuids::uuid{};
        return part;
    };

    std::map<std::string, std::string> out;
    for (const auto& row : bundle.rows) {
        if (row.family != "convention")
            continue;
        const auto c = unpack_row<refdata::domain::convention>(row);
        if (c.identity.kind_code != commodity_future_kind)
            continue;
        auto item = parts.item_of(c, without_keys);
        item.key.id = boost::uuids::uuid{};
        item.convention->identity.id = boost::uuids::uuid{};
        out[c.identity.kind_code + "\x1f" + c.payload.name] = ores::utility::codec::json::write(item);
    }
    return out;
}

}

}
