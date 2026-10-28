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
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

std::string join_fields(const std::vector<std::string>& fields, char sep) {
    std::string out;
    for (const auto& f : fields) {
        if (!out.empty())
            out += sep;
        out += f;
    }
    return out;
}

std::string date_field(const record_reader& v, std::string_view name) {
    const auto x = v.text(name);
    if (!x)
        return {};
    return x->empty() ? std::string("~") : *x;
}

std::string flag_field(const record_reader& v, std::string_view name) {
    const auto x = v.flag(name);
    return x ? truth(*x) : std::string();
}

std::string optional_text_field(const record_reader& v, std::string_view name) {
    return v.text(name).value_or(std::string());
}

std::string optional_spelling_field(const record_reader& v, std::string_view name) {
    return v.spelling(name).value_or(std::string());
}

std::string serialize_schedule_rule(const record_reader& rule) {
    return join_fields({text_of(rule, "StartDate"),
                        date_field(rule, "EndDate"),
                        flag_field(rule, "AdjustEndDateToPreviousMonthEnd"),
                        text_of(rule, "Tenor"),
                        optional_text_field(rule, "Calendar"),
                        optional_spelling_field(rule, "Convention"),
                        optional_spelling_field(rule, "TermConvention"),
                        optional_spelling_field(rule, "Rule"),
                        flag_field(rule, "EndOfMonth"),
                        optional_spelling_field(rule, "EndOfMonthConvention"),
                        date_field(rule, "FirstDate"),
                        date_field(rule, "LastDate"),
                        text_flag_field(rule, "RemoveFirstDate"),
                        text_flag_field(rule, "RemoveLastDate")},
                       '|');
}

void set_date_field(record_writer& w, std::string_view name, const std::string& field) {
    if (field == "~")
        w.set_text(name, "");
    else if (!field.empty())
        w.set_text(name, field);
}

void set_flag_field(record_writer& w, std::string_view name, const std::string& field) {
    if (!field.empty())
        w.set_flag(name, field == "true");
}

void set_text_flag_field(record_writer& w, std::string_view name, const std::string& field) {
    if (!field.empty())
        w.set_text(name, field);
}

void set_spelling_field(record_writer& w,
                        std::string_view name,
                        const std::string& field,
                        std::string_view what) {
    if (!field.empty())
        w.set_spelling(name, field, what);
}

void write_schedule_rule(const std::string& s, record_writer rule) {
    const auto f = split_fields(s, '|');
    auto at = [&f](std::size_t i) {
        return i < f.size() ? f[i] : std::string();
    };
    rule.set_text("StartDate", at(0));
    set_date_field(rule, "EndDate", at(1));
    set_flag_field(rule, "AdjustEndDateToPreviousMonthEnd", at(2));
    rule.set_text("Tenor", at(3));
    if (!at(4).empty())
        rule.set_text("Calendar", at(4));
    set_spelling_field(rule, "Convention", at(5), "business day convention");
    set_spelling_field(rule, "TermConvention", at(6), "business day convention");
    set_spelling_field(rule, "Rule", at(7), "date rule");
    set_flag_field(rule, "EndOfMonth", at(8));
    set_spelling_field(rule, "EndOfMonthConvention", at(9), "business day convention");
    set_date_field(rule, "FirstDate", at(10));
    set_date_field(rule, "LastDate", at(11));
    set_text_flag_field(rule, "RemoveFirstDate", at(12));
    set_text_flag_field(rule, "RemoveLastDate", at(13));
}

std::string serialize_schedule_dates(const record_reader& block) {
    std::string dates;
    if (const auto list = block.child("Dates")) {
        for (const auto& d : list->values("Date")) {
            if (!dates.empty())
                dates += ",";
            dates += d;
        }
    }
    return join_fields({optional_text_field(block, "Calendar"),
                        optional_spelling_field(block, "Convention"),
                        optional_text_field(block, "Tenor"),
                        flag_field(block, "EndOfMonth"),
                        flag_field(block, "IncludeDuplicateDates"),
                        dates},
                       '|');
}

void write_schedule_dates(const std::string& s, record_writer block) {
    const auto f = split_fields(s, '|');
    auto at = [&f](std::size_t i) {
        return i < f.size() ? f[i] : std::string();
    };
    if (!at(0).empty())
        block.set_text("Calendar", at(0));
    set_spelling_field(block, "Convention", at(1), "business day convention");
    if (!at(2).empty())
        block.set_text("Tenor", at(2));
    set_flag_field(block, "EndOfMonth", at(3));
    set_flag_field(block, "IncludeDuplicateDates", at(4));
    auto dates = block.child("Dates");
    for (const auto& d : split_fields(at(5), ',')) {
        if (!d.empty())
            dates.append_value("Date", d);
    }
}

std::string serialize_derived_schedule(const record_reader& d) {
    return join_fields({text_of(d, "BaseSchedule"),
                        optional_text_field(d, "Shift"),
                        optional_text_field(d, "Calendar"),
                        optional_spelling_field(d, "Convention"),
                        text_flag_field(d, "RemoveFirstDate"),
                        text_flag_field(d, "RemoveLastDate"),
                        optional_spelling_field(d, "ShiftUnit"),
                        optional_spelling_field(d, "ShiftAnchor")},
                       '|');
}

void write_derived_schedule(const std::string& s, record_writer d) {
    const auto f = split_fields(s, '|');
    auto at = [&f](std::size_t i) {
        return i < f.size() ? f[i] : std::string();
    };
    d.set_text("BaseSchedule", at(0));
    if (!at(1).empty())
        d.set_text("Shift", at(1));
    if (!at(2).empty())
        d.set_text("Calendar", at(2));
    set_spelling_field(d, "Convention", at(3), "business day convention");
    set_text_flag_field(d, "RemoveFirstDate", at(4));
    set_text_flag_field(d, "RemoveLastDate", at(5));
    set_spelling_field(d, "ShiftUnit", at(6), "shift unit");
    set_spelling_field(d, "ShiftAnchor", at(7), "shift anchor");
}

}

refdata::domain::inflation_swap_convention
conventions_mapper::map_inflation_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping inflation swap convention: " << text_of(v, "Id");

    refdata::domain::inflation_swap_convention r;
    r.id = text_of(v, "Id");
    r.fix_calendar = text_of(v, "FixCalendar");
    r.fix_convention = bdc_of(v, "FixConvention");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    r.index = text_of(v, "Index");
    r.interpolated = flag_of(v, "Interpolated");
    r.observation_lag = text_of(v, "ObservationLag");
    r.adjust_inflation_observation_dates = flag_of(v, "AdjustInflationObservationDates");
    r.inflation_calendar = text_of(v, "InflationCalendar");
    r.inflation_convention = bdc_of(v, "InflationConvention");
    assign(r.publication_roll, opt_spelling(v, "PublicationRoll"));
    assign(r.start_delay, opt_text(v, "StartDelay"));
    assign(r.start_delay_convention, opt_bdc(v, "StartDelayConvention"));

    if (const auto s = v.child("PublicationSchedule")) {
        assign(r.publication_schedule_name, opt_text(*s, "name"));

        std::string rules;
        for (const auto& rule : s->children("Rules")) {
            if (!rules.empty())
                rules += ";";
            rules += serialize_schedule_rule(rule);
        }
        if (!rules.empty())
            r.publication_schedule_rules = rules;

        std::string dates;
        for (const auto& block : s->children("Dates")) {
            if (!dates.empty())
                dates += ";";
            dates += serialize_schedule_dates(block);
        }
        if (!dates.empty())
            r.publication_schedule_dates = dates;

        std::string groups;
        for (const auto& d : s->children("DerivedSchedule")) {
            if (!groups.empty())
                groups += ";";
            groups += "S|" + serialize_derived_schedule(d);
        }
        for (const auto& d : s->children("Derived")) {
            if (!groups.empty())
                groups += ";";
            groups += "D|" + serialize_derived_schedule(d);
        }
        if (!groups.empty())
            r.publication_schedule_derived_groups = groups;
    }

    set_audit(r);
    return r;
}

void conventions_mapper::reverse_inflation_swap(const refdata::domain::inflation_swap_convention& v,
                                                record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("FixCalendar", v.fix_calendar);
    put_bdc(w, "FixConvention", v.fix_convention);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_text("Index", v.index);
    w.set_flag("Interpolated", v.interpolated);
    w.set_text("ObservationLag", v.observation_lag);
    w.set_flag("AdjustInflationObservationDates", v.adjust_inflation_observation_dates);
    w.set_text("InflationCalendar", v.inflation_calendar);
    put_bdc(w, "InflationConvention", v.inflation_convention);
    put_enum(w, "PublicationRoll", v.publication_roll, "publication roll");
    w.set_optional_text("StartDelay", v.start_delay);
    put_bdc(w, "StartDelayConvention", v.start_delay_convention);
    if (v.publication_schedule_name || v.publication_schedule_rules ||
        v.publication_schedule_dates || v.publication_schedule_derived_groups) {
        auto schedule = w.child("PublicationSchedule");
        schedule.set_optional_text("name", v.publication_schedule_name);
        if (v.publication_schedule_rules) {
            for (const auto& token : split_fields(*v.publication_schedule_rules, ';')) {
                if (!token.empty())
                    write_schedule_rule(token, schedule.append("Rules"));
            }
        }
        if (v.publication_schedule_dates) {
            for (const auto& token : split_fields(*v.publication_schedule_dates, ';')) {
                if (!token.empty())
                    write_schedule_dates(token, schedule.append("Dates"));
            }
        }
        if (v.publication_schedule_derived_groups) {
            for (const auto& token : split_fields(*v.publication_schedule_derived_groups, ';')) {
                if (token.size() < 3 || token[1] != '|')
                    continue;
                write_derived_schedule(
                    token.substr(2),
                    schedule.append(token[0] == 'S' ? "DerivedSchedule" : "Derived"));
            }
        }
    }
}

namespace conventions_family {

void pack_inflation_swap(const record_reader& root, refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "inflation_swap",
                root.children("InflationSwap"),
                &conventions_mapper::map_inflation_swap);
}

void join_inflation_swap(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_inflation_swap(
        unpack_row<refdata::domain::inflation_swap_convention>(row), root.append("InflationSwap"));
}

}

}
