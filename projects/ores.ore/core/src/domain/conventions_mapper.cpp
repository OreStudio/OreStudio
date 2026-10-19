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
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <algorithm>
#include <cmath>
#include <format>
#include <functional>
#include <map>
#include <rfl/json.hpp>
#include <sstream>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

namespace xg = xml::generated::conventions;
using xml::record_reader;
using xml::record_writer;

// Each table maps every spelling the schema allows for an enumeration to the
// canonical code the refdata columns hold.
using code_table = std::map<std::string, std::string, std::less<>>;

const code_table business_day_conventions = {
    {"F", "Following"},
    {"Following", "Following"},
    {"FOLLOWING", "Following"},
    {"MF", "ModifiedFollowing"},
    {"ModifiedFollowing", "ModifiedFollowing"},
    {"Modified Following", "ModifiedFollowing"},
    {"MODIFIEDF", "ModifiedFollowing"},
    {"MODFOLLOWING", "ModifiedFollowing"},
    {"P", "Preceding"},
    {"Preceding", "Preceding"},
    {"PRECEDING", "Preceding"},
    {"MP", "ModifiedPreceding"},
    {"ModifiedPreceding", "ModifiedPreceding"},
    {"Modified Preceding", "ModifiedPreceding"},
    {"MODIFIEDP", "ModifiedPreceding"},
    {"HMMF", "HalfMonthModifiedFollowing"},
    {"HalfMonthModifiedFollowing", "HalfMonthModifiedFollowing"},
    {"HalfMonthMF", "HalfMonthModifiedFollowing"},
    {"Half Month Modified Following", "HalfMonthModifiedFollowing"},
    {"HALFMONTHMF", "HalfMonthModifiedFollowing"},
    {"NEAREST", "Nearest"},
    {"NONE", "Unadjusted"},
    {"NotApplicable", "Unadjusted"},
    {"U", "Unadjusted"},
    {"Unadjusted", "Unadjusted"},
    {"INDIFF", "Unadjusted"},
};

const code_table day_counters = {
    {"A360", "ACT/360"},
    {"Actual/360", "ACT/360"},
    {"ACT/360", "ACT/360"},
    {"Act/360", "ACT/360"},
    {"A360 (Incl Last)", "ACT/360 (incl. last)"},
    {"Actual/360 (Incl Last)", "ACT/360 (incl. last)"},
    {"ACT/360 (Incl Last)", "ACT/360 (incl. last)"},
    {"A365", "ACT/365.FIXED"},
    {"A365F", "ACT/365.FIXED"},
    {"Actual/365 (Fixed)", "ACT/365.FIXED"},
    {"Actual/365 (fixed)", "ACT/365.FIXED"},
    {"ACT/365.FIXED", "ACT/365.FIXED"},
    {"ACT/365", "ACT/365.FIXED"},
    {"Act/365", "ACT/365.FIXED"},
    {"ACT/365L", "ACT/365L"},
    {"Act/365L", "ACT/365L"},
    {"Act/365 (Canadian Bond)", "ACT/365 (Canadian Bond)"},
    {"T360", "T360"},
    {"30/360", "30/360"},
    {"30/360 US", "30/360"},
    {"30/360 (US)", "30/360"},
    {"30/360 NASD", "30/360"},
    {"30U/360", "30/360"},
    {"30US/360", "30/360"},
    {"30/360 (Bond Basis)", "30/360"},
    {"ACT/nACT", "ACT/nACT"},
    {"30E/360 (Eurobond Basis)", "30E/360"},
    {"30/360 AIBD (Euro)", "30E/360"},
    {"30E/360.ICMA", "30E/360"},
    {"30E/360 ICMA", "30E/360"},
    {"30E/360", "30E/360"},
    {"30E/360E", "30E/360"},
    {"30E/360.ISDA", "30E/360.ISDA"},
    {"30E/360 ISDA", "30E/360.ISDA"},
    {"30/360 German", "30/360 (German)"},
    {"30/360 (German)", "30/360 (German)"},
    {"30/360 Italian", "30/360 (Italian)"},
    {"30/360 (Italian)", "30/360 (Italian)"},
    {"ActActISDA", "ACT/ACT.ISDA"},
    {"ACT/ACT.ISDA", "ACT/ACT.ISDA"},
    {"Actual/Actual (ISDA)", "ACT/ACT.ISDA"},
    {"ActualActual (ISDA)", "ACT/ACT.ISDA"},
    {"ACT/ACT", "ACT/ACT.ISDA"},
    {"Act/Act", "ACT/ACT.ISDA"},
    {"ACT29", "ACT/ACT.ISDA"},
    {"ACT", "ACT/ACT.ISDA"},
    {"ActActISMA", "ACT/ACT.ISMA"},
    {"Actual/Actual (ISMA)", "ACT/ACT.ISMA"},
    {"ActualActual (ISMA)", "ACT/ACT.ISMA"},
    {"ACT/ACT.ISMA", "ACT/ACT.ISMA"},
    {"ActActICMA", "ACT/ACT.ISMA"},
    {"Actual/Actual (ICMA)", "ACT/ACT.ISMA"},
    {"ActualActual (ICMA)", "ACT/ACT.ISMA"},
    {"ACT/ACT.ICMA", "ACT/ACT.ISMA"},
    {"ActActAFB", "ACT/ACT.AFB"},
    {"ACT/ACT.AFB", "ACT/ACT.AFB"},
    {"Actual/Actual (AFB)", "ACT/ACT.AFB"},
    {"1/1", "1/1"},
    {"BUS/252", "BUS/252"},
    {"Business/252", "BUS/252"},
    {"Actual/365 (No Leap)", "NL/365"},
    {"Act/365 (NL)", "NL/365"},
    {"NL/365", "NL/365"},
    {"Actual/365 (JGB)", "ACT/365 (JGB)"},
    {"Simple", "Simple"},
    {"Year", "Year"},
    {"A364", "ACT/364"},
    {"Actual/364", "ACT/364"},
    {"Act/364", "ACT/364"},
    {"ACT/364", "ACT/364"},
    {"Month", "Month"},
};

const code_table frequencies = {
    {"Z", "Once"},
    {"Once", "Once"},
    {"A", "Annual"},
    {"Annual", "Annual"},
    {"S", "Semiannual"},
    {"Semiannual", "Semiannual"},
    {"Q", "Quarterly"},
    {"Quarterly", "Quarterly"},
    {"B", "Bimonthly"},
    {"Bimonthly", "Bimonthly"},
    {"M", "Monthly"},
    {"Monthly", "Monthly"},
    {"L", "Lunarmonth"},
    {"Lunarmonth", "Lunarmonth"},
    {"W", "Weekly"},
    {"Weekly", "Weekly"},
    {"D", "Daily"},
    {"Daily", "Daily"},
};

const code_table compoundings = {
    {"Simple", "Simple"},
    {"Compounded", "Compounded"},
    {"Continuous", "Continuous"},
    {"SimpleThenCompounded", "SimpleThenCompounded"},
};

const code_table date_rules = {
    {"Backward", "Backward"},
    {"Forward", "Forward"},
    {"Zero", "Zero"},
    {"ThirdWednesday", "ThirdWednesday"},
    {"Twentieth", "Twentieth"},
    {"TwentiethIMM", "TwentiethIMM"},
    {"OldCDS", "OldCDS"},
    {"CDS", "CDS"},
    {"CDS2015", "CDS2015"},
    {"ThirdThursday", "ThirdThursday"},
    {"ThirdFriday", "ThirdFriday"},
    {"MondayAfterThirdFriday", "MondayAfterThirdFriday"},
    {"TuesdayAfterThirdFriday", "TuesdayAfterThirdFriday"},
    {"LastWednesday", "LastWednesday"},
    {"EveryThursday", "EveryThursday"},
};

// A day counter's canonical code is not always a spelling the schema allows,
// so writing one back needs the spelling. A code that is already a spelling
// is written as it stands.
const std::map<std::string, std::string, std::less<>> day_counter_spellings = {
    {"ACT/360", "Actual/360"},
    {"ACT/360 (incl. last)", "A360 (Incl Last)"},
    {"ACT/365.FIXED", "A365F"},
    {"ACT/365 (Canadian Bond)", "Act/365 (Canadian Bond)"},
    {"30/360 (German)", "30/360 German"},
    {"30/360 (Italian)", "30/360 Italian"},
    {"ACT/ACT.ISDA", "ActActISDA"},
    {"ACT/ACT.ISMA", "ActActISMA"},
    {"ACT/ACT.AFB", "ActActAFB"},
    {"ACT/365 (JGB)", "Actual/365 (JGB)"},
    {"ACT/364", "A364"},
};

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

constexpr std::string_view tenor_basis_swap_kind = "TENOR_BASIS_SWAP";

// A convention is identified by its party, its kind and the name it carries in
// the source document. The mapper knows the kind and the name but not the
// party, so it cannot state that identity and does not try: the store resolves
// it once the session's party is known. What the mapper emits instead is a
// provisional key, minted fresh for each convention it maps, so the part rows
// can reach their central row inside one mapped document. The store discards
// the provisional key and writes the row under the key the identity resolves
// to, so the provisional key is never an identity.
boost::uuids::uuid provisional_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

// ---------------------------------------------------------------------------
// Reading a record
//
// A member the document leaves out reads as the value an unset member has, as
// the generated binding it replaces did: an empty text, a zero, a false. A
// member that has no such value, an enumeration, stops with its name.
// ---------------------------------------------------------------------------

std::string text_of(const record_reader& v, std::string_view name) {
    return v.text(name).value_or(std::string());
}

std::optional<std::string> opt_text(const record_reader& v, std::string_view name) {
    return v.text(name);
}

int integer_of(const record_reader& v, std::string_view name) {
    return static_cast<int>(v.integer(name).value_or(0));
}

std::optional<int> opt_integer(const record_reader& v, std::string_view name) {
    if (const auto x = v.integer(name))
        return static_cast<int>(*x);
    return std::nullopt;
}

bool flag_of(const record_reader& v, std::string_view name) {
    return v.flag(name).value_or(false);
}

std::optional<bool> opt_flag(const record_reader& v, std::string_view name) {
    return v.flag(name);
}

std::optional<double> opt_real(const record_reader& v, std::string_view name) {
    return v.real(name);
}

double decimal_of(const record_reader& v, std::string_view name) {
    if (const auto x = v.decimal(name))
        return x->to_double();
    return 0.0;
}

std::optional<double> opt_decimal(const record_reader& v, std::string_view name) {
    if (const auto x = v.decimal(name))
        return x->to_double();
    return std::nullopt;
}

std::string code_of(const code_table& table, std::string_view spelling, std::string_view what) {
    const auto it = table.find(spelling);
    if (it == table.end())
        throw std::runtime_error(std::format("Unknown {} enum value", what));
    return it->second;
}

std::string coded(const record_reader& v,
                  std::string_view name,
                  const code_table& table,
                  std::string_view what) {
    const auto spelling = v.spelling(name);
    if (!spelling)
        throw std::runtime_error(std::format("Unknown {} enum value", what));
    return code_of(table, *spelling, what);
}

std::optional<std::string> opt_coded(const record_reader& v,
                                     std::string_view name,
                                     const code_table& table,
                                     std::string_view what) {
    const auto spelling = v.spelling(name);
    if (!spelling)
        return std::nullopt;
    return code_of(table, *spelling, what);
}

std::string bdc_of(const record_reader& v, std::string_view name) {
    return coded(v, name, business_day_conventions, "business day convention");
}

std::optional<std::string> opt_bdc(const record_reader& v, std::string_view name) {
    return opt_coded(v, name, business_day_conventions, "business day convention");
}

std::string day_counter_of(const record_reader& v, std::string_view name) {
    return coded(v, name, day_counters, "day counter");
}

std::optional<std::string> opt_day_counter(const record_reader& v, std::string_view name) {
    return opt_coded(v, name, day_counters, "day counter");
}

std::string frequency_of(const record_reader& v, std::string_view name) {
    return coded(v, name, frequencies, "frequency type");
}

std::optional<std::string> opt_frequency(const record_reader& v, std::string_view name) {
    return opt_coded(v, name, frequencies, "frequency type");
}

std::optional<std::string> opt_compounding(const record_reader& v, std::string_view name) {
    return opt_coded(v, name, compoundings, "compounding");
}

std::optional<std::string> opt_date_rule(const record_reader& v, std::string_view name) {
    return opt_coded(v, name, date_rules, "date rule");
}

// The schema's own spelling of an enumeration whose spelling is also the code.
std::string spelling_of(const record_reader& v, std::string_view name, std::string_view what) {
    const auto spelling = v.spelling(name);
    if (!spelling)
        throw std::runtime_error(std::format("Unknown {} enum value", what));
    return *spelling;
}

std::optional<std::string> opt_spelling(const record_reader& v, std::string_view name) {
    return v.spelling(name);
}

// Assigns to a member that is a value or an optional, only when the document
// states one.
template <typename Target, typename Value>
void assign(Target& target, const std::optional<Value>& value) {
    if (value)
        target = *value;
}

bool is_true(std::string_view text) {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
}

std::string join_fields(const std::vector<std::string>& fields, char sep) {
    std::string out;
    for (const auto& f : fields) {
        if (!out.empty())
            out += sep;
        out += f;
    }
    return out;
}

std::vector<std::string> split_fields(const std::string& s, char sep) {
    std::vector<std::string> out;
    std::istringstream in(s);
    std::string token;
    while (std::getline(in, token, sep))
        out.push_back(token);
    return out;
}

std::string join_with(const std::vector<std::string>& parts, char sep) {
    std::string out;
    for (std::size_t i = 0; i < parts.size(); ++i) {
        if (i != 0)
            out += sep;
        out += parts[i];
    }
    return out;
}

std::string truth(bool v) {
    return v ? "true" : "false";
}

// ---------------------------------------------------------------------------
// Schedules and load profiles
//
// The refdata columns hold these as delimited text, one field per member the
// document states. An empty field is a member the document left out, and a
// tilde is a date the document states and leaves empty.
// ---------------------------------------------------------------------------

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

std::string text_flag_field(const record_reader& v, std::string_view name) {
    const auto x = v.text(name);
    return x ? truth(is_true(*x)) : std::string();
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

std::string serialize_factors(const std::optional<record_reader>& factors) {
    std::string out;
    if (!factors)
        return out;
    for (const auto& f : factors->children("LoadFactor")) {
        if (!out.empty())
            out += ",";
        out += text_of(f, "from") + ":" + text_of(f, "to") + ":" + text_of(f, "unit") + ":" +
               text_flag_field(f, "dst") + ":" + f.own_text().value_or(std::string());
    }
    return out;
}

void write_factors(const std::string& s, record_writer factors) {
    for (const auto& record : split_fields(s, ',')) {
        if (record.empty())
            continue;
        const auto parts = split_fields(record, ':');
        if (parts.size() < 5)
            continue;
        auto f = factors.append("LoadFactor");
        f.set_text("from", parts[0]);
        f.set_text("to", parts[1]);
        if (!parts[2].empty())
            f.set_text("unit", parts[2]);
        if (!parts[3].empty())
            f.set_text("dst", parts[3]);
        f.set_own_text(parts[4]);
    }
}

// ---------------------------------------------------------------------------
// Writing a record
// ---------------------------------------------------------------------------

std::optional<std::int64_t> as_integer(const std::optional<int>& v) {
    if (v)
        return static_cast<std::int64_t>(*v);
    return std::nullopt;
}

std::string bdc_spelling(const std::string& code) {
    return code == "Nearest" ? std::string("NEAREST") : code;
}

std::string day_counter_spelling(const std::string& code) {
    const auto it = day_counter_spellings.find(code);
    return it == day_counter_spellings.end() ? code : it->second;
}

void put_bdc(record_writer& w, std::string_view name, const std::string& code) {
    w.set_spelling(name, bdc_spelling(code), "business day convention");
}

void put_bdc(record_writer& w, std::string_view name, const std::optional<std::string>& code) {
    if (code)
        put_bdc(w, name, *code);
}

void put_day_counter(record_writer& w, std::string_view name, const std::string& code) {
    w.set_spelling(name, day_counter_spelling(code), "day counter");
}

void put_day_counter(record_writer& w,
                     std::string_view name,
                     const std::optional<std::string>& code) {
    if (code)
        put_day_counter(w, name, *code);
}

void put_enum(record_writer& w, std::string_view name, const std::string& value, std::string_view what) {
    w.set_spelling(name, value, what);
}

void put_enum(record_writer& w,
              std::string_view name,
              const std::optional<std::string>& value,
              std::string_view what) {
    if (value)
        w.set_spelling(name, *value, what);
}

void put_decimal(record_writer& w, std::string_view name, double value) {
    const auto exact = ores::utility::decimal::decimal::from_double(value);
    if (!exact)
        throw std::runtime_error(std::format("'{}' is not an exact decimal: {}", name, exact.error()));
    w.set_decimal(name, *exact);
}

void put_decimal(record_writer& w, std::string_view name, const std::optional<double>& value) {
    if (value)
        put_decimal(w, name, *value);
}

// ---------------------------------------------------------------------------
// Individual type reverse mappers
//
// Each fills the record of one document element, which the caller appended.
// ---------------------------------------------------------------------------

void reverse_zero(const refdata::domain::zero_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_flag("TenorBased", v.tenor_based);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    put_enum(w, "Compounding", v.compounding, "compounding");
    put_enum(w, "CompoundingFrequency", v.compounding_frequency, "frequency type");
    w.set_optional_text("TenorCalendar", v.tenor_calendar);
    w.set_optional_integer("SpotLag", as_integer(v.spot_lag));
    w.set_optional_text("SpotCalendar", v.spot_calendar);
    put_bdc(w, "RollConvention", v.roll_convention);
    w.set_optional_flag("EOM", v.end_of_month);
}

void reverse_deposit(const refdata::domain::deposit_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_flag("IndexBased", v.index_based);
    w.set_optional_text("Index", v.index);
    w.set_optional_text("Calendar", v.calendar);
    put_bdc(w, "Convention", v.convention);
    w.set_optional_flag("EOM", v.end_of_month);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_optional_integer("SettlementDays", as_integer(v.settlement_days));
}

void reverse_swap(const refdata::domain::swap_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_optional_text("FixedCalendar", v.fixed_calendar);
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_text("Index", v.index);
    put_enum(w, "FloatFrequency", v.float_frequency, "frequency type");
    put_enum(w, "SubPeriodsCouponType", v.sub_periods_coupon_type, "sub-periods coupon type");
}

void reverse_ois(const refdata::domain::ois_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SpotLag", v.spot_lag);
    w.set_text("Index", v.index);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_optional_text("FixedCalendar", v.fixed_calendar);
    w.set_optional_integer("PaymentLag", as_integer(v.payment_lag));
    w.set_optional_flag("EOM", v.end_of_month);
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_bdc(w, "FixedPaymentConvention", v.fixed_payment_convention);
    put_enum(w, "Rule", v.rule, "date rule");
    w.set_optional_text("PaymentCalendar", v.payment_calendar);
    w.set_optional_integer("RateCutoff", as_integer(v.rate_cutoff));
}

void reverse_fra(const refdata::domain::fra_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Index", v.index);
}

void reverse_ibor_index(const refdata::domain::ibor_index_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("FixingCalendar", v.fixing_calendar);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_integer("SettlementDays", v.settlement_days);
    put_bdc(w, "BusinessDayConvention", v.business_day_convention);
    w.set_flag("EndOfMonth", v.end_of_month);
}

void reverse_commodity_forward(const refdata::domain::commodity_forward_convention& v,
                               record_writer w) {
    w.set_text("Id", v.id);
    w.set_optional_integer("SpotDays", as_integer(v.spot_days));
    put_decimal(w, "PointsFactor", v.points_factor);
    w.set_optional_text("AdvanceCalendar", v.advance_calendar);
    w.set_optional_flag("SpotRelative", v.spot_relative);
    w.set_optional_text("DeliveryLocation", v.delivery_location);
    put_bdc(w, "BusinessDayConvention", v.business_day_convention);
    w.set_optional_flag("Outright", v.outright);
}

void reverse_intraday_power_load(const refdata::domain::intraday_power_load_convention& v,
                                 record_writer w) {
    w.set_text("Id", v.id);
    if (!v.explicit_load_profile && !v.business_day_load_rules)
        return;

    auto profile = w.child("PowerLoadProfileData");
    if (v.explicit_load_profile) {
        auto explicit_dates = profile.child("ExplicitDates");
        for (const auto& record : split_fields(*v.explicit_load_profile, ';')) {
            if (record.empty())
                continue;
            const auto parts = split_fields(record, '|');
            auto datum = explicit_dates.append("LoadProfileDatum");
            datum.set_text("Date", parts.empty() ? std::string() : parts[0]);
            auto factors = datum.child("LoadFactors");
            if (parts.size() > 1)
                write_factors(parts[1], factors);
        }
    }
    if (v.business_day_load_rules) {
        auto rules = profile.child("BusinessDayRules");
        for (const auto& record : split_fields(*v.business_day_load_rules, ';')) {
            const auto parts = split_fields(record, '|');
            auto rule = rules.append("LoadProfileBusinessDayRule");
            rule.set_text("Date", parts.empty() ? std::string() : parts[0]);
            rule.set_text("Calendar", parts.size() > 1 ? parts[1] : std::string());
            if (parts.size() > 2 && !parts[2].empty())
                write_factors(parts[2], rule.child("BusinessDayLoadFactors"));
            if (parts.size() > 3 && !parts[3].empty())
                write_factors(parts[3], rule.child("NonBusinessDayLoadFactors"));
        }
    }
}

void reverse_bond_yield(const refdata::domain::bond_yield_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Compounding", v.compounding);
    put_enum(w, "Frequency", v.frequency, "frequency type");
    w.set_optional_text("PriceType", v.price_type);
    w.set_optional_real("Accuracy", v.accuracy);
    w.set_optional_integer("MaxEvaluations", as_integer(v.max_evaluations));
    w.set_optional_real("Guess", v.guess);
}

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

void reverse_commodity_future(const refdata::domain::commodity_future_convention& v,
                              record_writer w) {
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
    put_enum(w,
             "OptionExpiryWeeklyDayOfTheWeek",
             v.option_expiry_weekly_day_of_the_week,
             "weekday");
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

void reverse_cms_spread_option(const refdata::domain::cms_spread_option_convention& v,
                               record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("ForwardStart", v.forward_start);
    w.set_text("SpotDays", v.spot_days);
    w.set_text("SwapTenor", v.swap_tenor);
    w.set_integer("FixingDays", v.fixing_days);
    w.set_text("Calendar", v.calendar);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    put_bdc(w, "RollConvention", v.roll_convention);
}

void reverse_cross_currency_fix_float(
    const refdata::domain::cross_currency_fix_float_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SettlementDays", v.settlement_days);
    w.set_text("SettlementCalendar", v.settlement_calendar);
    put_bdc(w, "SettlementConvention", v.settlement_convention);
    put_enum(w, "FixedCurrency", v.fixed_currency, "currency code");
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_text("Index", v.index);
    w.set_optional_flag("EOM", v.eom);
    w.set_optional_flag("IsResettable", v.is_resettable);
    w.set_optional_flag("FloatIndexIsResettable", v.float_index_is_resettable);
    w.set_optional_flag("IncludeSpread", v.include_spread);
    w.set_optional_text("Lookback", v.lookback);
    w.set_optional_integer("FixingDays", as_integer(v.fixing_days));
    w.set_optional_integer("RateCutoff", as_integer(v.rate_cutoff));
    w.set_optional_flag("IsAveraged", v.is_averaged);
    w.set_optional_flag("ObservationShift", v.observation_shift);
}

void reverse_inflation_swap(const refdata::domain::inflation_swap_convention& v, record_writer w) {
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
                    token.substr(2), schedule.append(token[0] == 'S' ? "DerivedSchedule" : "Derived"));
            }
        }
    }
}

void reverse_bma_basis_swap(const refdata::domain::bma_basis_swap_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Index", v.index);
    w.set_text("BMAIndex", v.bma_index);
    w.set_optional_text("BMAPaymentCalendar", v.bma_payment_calendar);
    put_bdc(w, "BMAPaymentConvention", v.bma_payment_convention);
    w.set_optional_integer("BMAPaymentLag", as_integer(v.bma_payment_lag));
    w.set_optional_text("IndexPaymentCalendar", v.index_payment_calendar);
    put_bdc(w, "IndexPaymentConvention", v.index_payment_convention);
    w.set_optional_integer("IndexPaymentLag", as_integer(v.index_payment_lag));
    w.set_optional_integer("IndexSettlementDays", as_integer(v.index_settlement_days));
    w.set_optional_text("IndexPaymentPeriod", v.index_payment_period);
    w.set_optional_integer("OvernightLockoutDays", as_integer(v.overnight_lockout_days));
}

void reverse_zero_inflation_index(const refdata::domain::zero_inflation_index_convention& v,
                                  record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("RegionName", v.region_name);
    w.set_text("RegionCode", v.region_code);
    w.set_flag("Revised", v.revised);
    put_enum(w, "Frequency", v.frequency, "frequency type");
    w.set_text("AvailabilityLag", v.availability_lag);
    put_enum(w, "Currency", v.currency, "currency code");
}

void reverse_overnight_index(const refdata::domain::overnight_index_convention& v,
                             record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("FixingCalendar", v.fixing_calendar);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_integer("SettlementDays", v.settlement_days);
}

void reverse_tenor_basis_two_swap(const refdata::domain::tenor_basis_two_swap_convention& v,
                                  record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Calendar", v.calendar);
    put_enum(w, "LongFixedFrequency", v.long_fixed_frequency, "frequency type");
    put_bdc(w, "LongFixedConvention", v.long_fixed_convention);
    put_day_counter(w, "LongFixedDayCounter", v.long_fixed_day_count_fraction);
    w.set_text("LongIndex", v.long_index);
    put_enum(w, "ShortFixedFrequency", v.short_fixed_frequency, "frequency type");
    put_bdc(w, "ShortFixedConvention", v.short_fixed_convention);
    put_day_counter(w, "ShortFixedDayCounter", v.short_fixed_day_count_fraction);
    w.set_text("ShortIndex", v.short_index);
    w.set_optional_flag("LongMinusShort", v.long_minus_short);
}

void reverse_tenor_basis_swap(const ores::refdata::messaging::tenor_basis_swap_convention_item& v,
                              record_writer w) {
    if (v.convention)
        w.set_text("Id", v.convention->name);
    if (!v.pay_receive.empty()) {
        const auto& p = v.pay_receive.front();
        w.set_optional_text("PayIndex", p.pay_index);
        w.set_optional_text("PayFrequency", p.pay_frequency);
        w.set_optional_text("ReceiveIndex", p.receive_index);
        w.set_optional_text("ReceiveFrequency", p.receive_frequency);
        w.set_optional_flag("SpreadOnRec", p.spread_on_rec);
        w.set_optional_flag("IncludeSpread", p.include_spread);
        put_enum(w, "SubPeriodsCouponType", p.sub_periods_coupon_type, "sub-periods coupon type");
        w.set_optional_flag("PayIsAveraged", p.pay_is_averaged);
        w.set_optional_flag("RecIsAveraged", p.rec_is_averaged);
        return;
    }
    if (!v.long_short.empty()) {
        const auto& l = v.long_short.front();
        w.set_optional_text("LongIndex", l.long_index);
        w.set_optional_text("LongPayTenor", l.long_pay_tenor);
        w.set_optional_text("ShortIndex", l.short_index);
        w.set_optional_text("ShortPayTenor", l.short_pay_tenor);
        w.set_optional_flag("SpreadOnShort", l.spread_on_short);
        w.set_optional_flag("IncludeSpread", l.include_spread);
        put_enum(w, "SubPeriodsCouponType", l.sub_periods_coupon_type, "sub-periods coupon type");
    }
}

void reverse_cross_currency_basis(const refdata::domain::cross_currency_basis_convention& v,
                                  record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SettlementDays", v.settlement_days);
    w.set_optional_text("SettlementCalendar", v.settlement_calendar);
    put_bdc(w, "RollConvention", v.roll_convention);
    w.set_text("FlatIndex", v.flat_index);
    w.set_text("SpreadIndex", v.spread_index);
    w.set_optional_flag("EOM", v.eom);
    w.set_optional_flag("IsResettable", v.is_resettable);
    w.set_optional_flag("FlatIndexIsResettable", v.flat_index_is_resettable);
    w.set_optional_text("FlatTenor", v.flat_tenor);
    w.set_optional_text("SpreadTenor", v.spread_tenor);
    w.set_optional_integer("SpreadPaymentLag", as_integer(v.spread_payment_lag));
    w.set_optional_integer("FlatPaymentLag", as_integer(v.flat_payment_lag));
    w.set_optional_flag("SpreadIncludeSpread", v.spread_include_spread);
    w.set_optional_text("SpreadLookback", v.spread_lookback);
    w.set_optional_integer("SpreadFixingDays", as_integer(v.spread_fixing_days));
    w.set_optional_integer("SpreadRateCutoff", as_integer(v.spread_rate_cutoff));
    w.set_optional_flag("SpreadIsAveraged", v.spread_is_averaged);
    w.set_optional_flag("SpreadObservationShift", v.spread_observation_shift);
    w.set_optional_flag("FlatIncludeSpread", v.flat_include_spread);
    w.set_optional_text("FlatLookback", v.flat_lookback);
    w.set_optional_integer("FlatFixingDays", as_integer(v.flat_fixing_days));
    w.set_optional_integer("FlatRateCutoff", as_integer(v.flat_rate_cutoff));
    w.set_optional_flag("FlatIsAveraged", v.flat_is_averaged);
    w.set_optional_flag("FlatObservationShift", v.flat_observation_shift);
}

void reverse_average_ois(const refdata::domain::average_ois_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SpotLag", v.spot_lag);
    w.set_text("FixedTenor", v.fixed_tenor);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_optional_text("FixedCalendar", v.fixed_calendar);
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_bdc(w, "FixedPaymentConvention", v.fixed_payment_convention);
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    w.set_text("Index", v.index);
    w.set_text("OnTenor", v.on_tenor);
    w.set_text("RateCutoff", v.rate_cutoff);
}

void reverse_fx_option(const refdata::domain::fx_option_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_optional_text("FXConventionID", v.fx_convention_id);
    w.set_text("AtmType", v.atm_type);
    w.set_text("DeltaType", v.delta_type);
    w.set_optional_text("SwitchTenor", v.switch_tenor);
    w.set_optional_text("LongTermAtmType", v.long_term_atm_type);
    w.set_optional_text("LongTermDeltaType", v.long_term_delta_type);
    w.set_optional_text("RiskReversalInFavorOf", v.risk_reversal_in_favor_of);
    w.set_optional_text("ButterflyStyle", v.butterfly_style);
}

void reverse_future(const refdata::domain::future_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Index", v.index);
    put_enum(w, "DateGenerationRule", v.date_generation_rule, "future date generation rule");
    put_enum(w,
             "OvernightIndexFutureNettingType",
             v.netting_type,
             "overnight index future netting type");
    w.set_optional_text("Calendar", v.calendar);
    w.set_optional_text("OvernightIndexTenor", v.overnight_index_tenor);
}

void reverse_swap_index(const refdata::domain::swap_index_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Conventions", v.conventions);
    w.set_optional_text("FixingCalendar", v.fixing_calendar);
}

std::string fx_convention_id(const std::string& base_currency, const std::string& quote_currency) {
    return base_currency + "-" + quote_currency + "-FX-CONVENTIONS";
}

void reverse_fx(const ores::refdata::messaging::fx_convention& v, record_writer w) {
    w.set_text("Id", fx_convention_id(v.pair.base_currency, v.pair.quote_currency));
    w.set_integer("SpotDays", v.spot_days);
    put_enum(w, "SourceCurrency", v.pair.base_currency, "currency code");
    put_enum(w, "TargetCurrency", v.pair.quote_currency, "currency code");
    put_decimal(w, "PointsFactor", v.convention.pip_factor != 0.0 ? 1.0 / v.convention.pip_factor : 0.0);
    if (!v.advance_calendars.empty())
        w.set_text("AdvanceCalendar", join_with(v.advance_calendars, ','));
    w.set_optional_flag("SpotRelative", v.convention.spot_relative);
    w.set_optional_flag("EOM", v.convention.end_of_month);
    put_bdc(w, "Convention", v.convention.business_day_convention);
}

void reverse_cds(const refdata::domain::cds_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SettlementDays", v.settlement_days);
    w.set_text("Calendar", v.calendar);
    put_enum(w, "Frequency", v.frequency, "frequency type");
    put_bdc(w, "PaymentConvention", v.payment_convention);
    put_enum(w, "Rule", v.rule, "date rule");
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_optional_integer("UpfrontSettlementDays", as_integer(v.upfront_settlement_days));
    w.set_flag("SettlesAccrual", v.settles_accrual);
    w.set_flag("PaysAtDefaultTime", v.pays_at_default_time);
    put_day_counter(w, "LastPeriodDayCounter", v.last_period_day_count_fraction);
}

} // namespace

// ---------------------------------------------------------------------------
// Individual type mappers
// ---------------------------------------------------------------------------

refdata::domain::zero_convention conventions_mapper::map_zero(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping zero convention: " << text_of(v, "Id");

    refdata::domain::zero_convention r;
    r.id = text_of(v, "Id");
    r.tenor_based = flag_of(v, "TenorBased");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    assign(r.compounding, opt_compounding(v, "Compounding"));
    assign(r.compounding_frequency, opt_frequency(v, "CompoundingFrequency"));
    assign(r.tenor_calendar, opt_text(v, "TenorCalendar"));
    assign(r.spot_lag, opt_integer(v, "SpotLag"));
    assign(r.spot_calendar, opt_text(v, "SpotCalendar"));
    assign(r.roll_convention, opt_bdc(v, "RollConvention"));
    assign(r.end_of_month, opt_flag(v, "EOM"));
    set_audit(r);
    return r;
}

refdata::domain::deposit_convention conventions_mapper::map_deposit(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping deposit convention: " << text_of(v, "Id");

    refdata::domain::deposit_convention r;
    r.id = text_of(v, "Id");
    r.index_based = flag_of(v, "IndexBased");
    assign(r.index, opt_text(v, "Index"));
    assign(r.calendar, opt_text(v, "Calendar"));
    assign(r.convention, opt_bdc(v, "Convention"));
    assign(r.end_of_month, opt_flag(v, "EOM"));
    assign(r.day_count_fraction, opt_day_counter(v, "DayCounter"));
    assign(r.settlement_days, opt_integer(v, "SettlementDays"));
    set_audit(r);
    return r;
}

refdata::domain::swap_convention conventions_mapper::map_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping swap convention: " << text_of(v, "Id");

    refdata::domain::swap_convention r;
    r.id = text_of(v, "Id");
    assign(r.fixed_calendar, opt_text(v, "FixedCalendar"));
    r.fixed_frequency = frequency_of(v, "FixedFrequency");
    assign(r.fixed_convention, opt_bdc(v, "FixedConvention"));
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    r.index = text_of(v, "Index");
    assign(r.float_frequency, opt_frequency(v, "FloatFrequency"));
    assign(r.sub_periods_coupon_type, opt_spelling(v, "SubPeriodsCouponType"));
    set_audit(r);
    return r;
}

refdata::domain::swap_index_convention conventions_mapper::map_swap_index(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping swap index convention: " << text_of(v, "Id");

    refdata::domain::swap_index_convention r;
    r.id = text_of(v, "Id");
    r.conventions = text_of(v, "Conventions");
    assign(r.fixing_calendar, opt_text(v, "FixingCalendar"));
    set_audit(r);
    return r;
}

refdata::domain::future_convention conventions_mapper::map_future(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping future convention: " << text_of(v, "Id");

    refdata::domain::future_convention r;
    r.id = text_of(v, "Id");
    r.index = text_of(v, "Index");
    assign(r.date_generation_rule, opt_spelling(v, "DateGenerationRule"));
    assign(r.netting_type, opt_spelling(v, "OvernightIndexFutureNettingType"));
    assign(r.calendar, opt_text(v, "Calendar"));
    assign(r.overnight_index_tenor, opt_text(v, "OvernightIndexTenor"));
    set_audit(r);
    return r;
}

refdata::domain::tenor_basis_two_swap_convention
conventions_mapper::map_tenor_basis_two_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping two-tenor basis swap convention: " << text_of(v, "Id");

    refdata::domain::tenor_basis_two_swap_convention r;
    r.id = text_of(v, "Id");
    r.calendar = text_of(v, "Calendar");
    r.long_fixed_frequency = frequency_of(v, "LongFixedFrequency");
    r.long_fixed_convention = bdc_of(v, "LongFixedConvention");
    r.long_fixed_day_count_fraction = day_counter_of(v, "LongFixedDayCounter");
    r.long_index = text_of(v, "LongIndex");
    r.short_fixed_frequency = frequency_of(v, "ShortFixedFrequency");
    r.short_fixed_convention = bdc_of(v, "ShortFixedConvention");
    r.short_fixed_day_count_fraction = day_counter_of(v, "ShortFixedDayCounter");
    r.short_index = text_of(v, "ShortIndex");
    assign(r.long_minus_short, opt_flag(v, "LongMinusShort"));
    set_audit(r);
    return r;
}

ores::refdata::messaging::tenor_basis_swap_convention_item
conventions_mapper::map_tenor_basis_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping tenor basis swap convention: " << text_of(v, "Id");

    ores::refdata::messaging::tenor_basis_swap_convention_item r;
    refdata::domain::convention central;
    central.kind_code = std::string(tenor_basis_swap_kind);
    central.name = text_of(v, "Id");
    central.id = provisional_uuid();
    set_audit(central);
    r.key.id = central.id;
    r.convention = central;

    // The form comes from the leg fields alone. IncludeSpread and
    // SubPeriodsCouponType are written alongside both forms, so they must never
    // decide it.
    const bool pay_receive_form = v.has("PayIndex") || v.has("ReceiveIndex");
    if (pay_receive_form) {
        refdata::domain::tenor_basis_swap_pay_receive p;
        p.id = provisional_uuid();
        p.convention_id = central.id;
        assign(p.pay_index, opt_text(v, "PayIndex"));
        assign(p.pay_frequency, opt_text(v, "PayFrequency"));
        assign(p.receive_index, opt_text(v, "ReceiveIndex"));
        assign(p.receive_frequency, opt_text(v, "ReceiveFrequency"));
        assign(p.spread_on_rec, opt_flag(v, "SpreadOnRec"));
        assign(p.include_spread, opt_flag(v, "IncludeSpread"));
        assign(p.sub_periods_coupon_type, opt_spelling(v, "SubPeriodsCouponType"));
        assign(p.pay_is_averaged, opt_flag(v, "PayIsAveraged"));
        assign(p.rec_is_averaged, opt_flag(v, "RecIsAveraged"));
        set_audit(p);
        r.pay_receive.push_back(std::move(p));
        return r;
    }

    refdata::domain::tenor_basis_swap_long_short l;
    l.id = provisional_uuid();
    l.convention_id = central.id;
    assign(l.long_index, opt_text(v, "LongIndex"));
    assign(l.long_pay_tenor, opt_text(v, "LongPayTenor"));
    assign(l.short_index, opt_text(v, "ShortIndex"));
    assign(l.short_pay_tenor, opt_text(v, "ShortPayTenor"));
    assign(l.spread_on_short, opt_flag(v, "SpreadOnShort"));
    assign(l.include_spread, opt_flag(v, "IncludeSpread"));
    assign(l.sub_periods_coupon_type, opt_spelling(v, "SubPeriodsCouponType"));
    set_audit(l);
    r.long_short.push_back(std::move(l));
    return r;
}

refdata::domain::cross_currency_basis_convention
conventions_mapper::map_cross_currency_basis(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency basis convention: " << text_of(v, "Id");

    refdata::domain::cross_currency_basis_convention r;
    r.id = text_of(v, "Id");
    r.settlement_days = integer_of(v, "SettlementDays");
    assign(r.settlement_calendar, opt_text(v, "SettlementCalendar"));
    r.roll_convention = bdc_of(v, "RollConvention");
    r.flat_index = text_of(v, "FlatIndex");
    r.spread_index = text_of(v, "SpreadIndex");
    assign(r.eom, opt_flag(v, "EOM"));
    assign(r.is_resettable, opt_flag(v, "IsResettable"));
    assign(r.flat_index_is_resettable, opt_flag(v, "FlatIndexIsResettable"));
    assign(r.flat_tenor, opt_text(v, "FlatTenor"));
    assign(r.spread_tenor, opt_text(v, "SpreadTenor"));
    assign(r.spread_payment_lag, opt_integer(v, "SpreadPaymentLag"));
    assign(r.flat_payment_lag, opt_integer(v, "FlatPaymentLag"));
    assign(r.spread_include_spread, opt_flag(v, "SpreadIncludeSpread"));
    assign(r.spread_lookback, opt_text(v, "SpreadLookback"));
    assign(r.spread_fixing_days, opt_integer(v, "SpreadFixingDays"));
    assign(r.spread_rate_cutoff, opt_integer(v, "SpreadRateCutoff"));
    assign(r.spread_is_averaged, opt_flag(v, "SpreadIsAveraged"));
    assign(r.spread_observation_shift, opt_flag(v, "SpreadObservationShift"));
    assign(r.flat_include_spread, opt_flag(v, "FlatIncludeSpread"));
    assign(r.flat_lookback, opt_text(v, "FlatLookback"));
    assign(r.flat_fixing_days, opt_integer(v, "FlatFixingDays"));
    assign(r.flat_rate_cutoff, opt_integer(v, "FlatRateCutoff"));
    assign(r.flat_is_averaged, opt_flag(v, "FlatIsAveraged"));
    assign(r.flat_observation_shift, opt_flag(v, "FlatObservationShift"));
    set_audit(r);
    return r;
}

refdata::domain::average_ois_convention
conventions_mapper::map_average_ois(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping averaging OIS convention: " << text_of(v, "Id");

    refdata::domain::average_ois_convention r;
    r.id = text_of(v, "Id");
    r.spot_lag = integer_of(v, "SpotLag");
    r.fixed_tenor = text_of(v, "FixedTenor");
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    // ORE makes the first three of these required, so every document in the
    // corpus carries them. Only the frequency is optional, and the corpus never
    // sets it.
    r.fixed_calendar = text_of(v, "FixedCalendar");
    r.fixed_convention = bdc_of(v, "FixedConvention");
    r.fixed_payment_convention = bdc_of(v, "FixedPaymentConvention");
    assign(r.fixed_frequency, opt_frequency(v, "FixedFrequency"));
    r.index = text_of(v, "Index");
    r.on_tenor = text_of(v, "OnTenor");
    r.rate_cutoff = text_of(v, "RateCutoff");
    set_audit(r);
    return r;
}

refdata::domain::fx_option_convention conventions_mapper::map_fx_option(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX option convention: " << text_of(v, "Id");

    refdata::domain::fx_option_convention r;
    r.id = text_of(v, "Id");
    assign(r.fx_convention_id, opt_text(v, "FXConventionID"));
    r.atm_type = text_of(v, "AtmType");
    r.delta_type = text_of(v, "DeltaType");
    assign(r.switch_tenor, opt_text(v, "SwitchTenor"));
    assign(r.long_term_atm_type, opt_text(v, "LongTermAtmType"));
    assign(r.long_term_delta_type, opt_text(v, "LongTermDeltaType"));
    assign(r.risk_reversal_in_favor_of, opt_text(v, "RiskReversalInFavorOf"));
    assign(r.butterfly_style, opt_text(v, "ButterflyStyle"));
    set_audit(r);
    return r;
}

refdata::domain::ois_convention conventions_mapper::map_ois(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping OIS convention: " << text_of(v, "Id");

    refdata::domain::ois_convention r;
    r.id = text_of(v, "Id");
    r.spot_lag = integer_of(v, "SpotLag");
    r.index = text_of(v, "Index");
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    assign(r.fixed_calendar, opt_text(v, "FixedCalendar"));
    assign(r.payment_lag, opt_integer(v, "PaymentLag"));
    assign(r.end_of_month, opt_flag(v, "EOM"));
    assign(r.fixed_frequency, opt_frequency(v, "FixedFrequency"));
    assign(r.fixed_convention, opt_bdc(v, "FixedConvention"));
    assign(r.fixed_payment_convention, opt_bdc(v, "FixedPaymentConvention"));
    assign(r.rule, opt_date_rule(v, "Rule"));
    assign(r.payment_calendar, opt_text(v, "PaymentCalendar"));
    assign(r.rate_cutoff, opt_integer(v, "RateCutoff"));
    set_audit(r);
    return r;
}

refdata::domain::fra_convention conventions_mapper::map_fra(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FRA convention: " << text_of(v, "Id");

    refdata::domain::fra_convention r;
    r.id = text_of(v, "Id");
    r.index = text_of(v, "Index");
    set_audit(r);
    return r;
}

refdata::domain::ibor_index_convention conventions_mapper::map_ibor_index(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping IBOR index convention: " << text_of(v, "Id");

    refdata::domain::ibor_index_convention r;
    r.id = text_of(v, "Id");
    r.fixing_calendar = text_of(v, "FixingCalendar");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    r.settlement_days = integer_of(v, "SettlementDays");
    r.business_day_convention = bdc_of(v, "BusinessDayConvention");
    r.end_of_month = flag_of(v, "EndOfMonth");
    set_audit(r);
    return r;
}

refdata::domain::commodity_forward_convention
conventions_mapper::map_commodity_forward(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping commodity forward convention: " << text_of(v, "Id");

    refdata::domain::commodity_forward_convention r;
    r.id = text_of(v, "Id");
    assign(r.spot_days, opt_integer(v, "SpotDays"));
    assign(r.points_factor, opt_decimal(v, "PointsFactor"));
    assign(r.advance_calendar, opt_text(v, "AdvanceCalendar"));
    assign(r.spot_relative, opt_flag(v, "SpotRelative"));
    assign(r.delivery_location, opt_text(v, "DeliveryLocation"));
    assign(r.business_day_convention, opt_bdc(v, "BusinessDayConvention"));
    assign(r.outright, opt_flag(v, "Outright"));
    set_audit(r);
    return r;
}

refdata::domain::intraday_power_load_convention
conventions_mapper::map_intraday_power_load(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping intraday power load convention: " << text_of(v, "Id");

    refdata::domain::intraday_power_load_convention r;
    r.id = text_of(v, "Id");

    if (const auto profile = v.child("PowerLoadProfileData")) {
        if (const auto explicit_dates = profile->child("ExplicitDates")) {
            std::string out;
            for (const auto& datum : explicit_dates->children("LoadProfileDatum")) {
                if (!out.empty())
                    out += ";";
                out += text_of(datum, "Date") + "|" +
                       serialize_factors(datum.child("LoadFactors"));
            }
            r.explicit_load_profile = out;
        }
        if (const auto rules = profile->child("BusinessDayRules")) {
            std::string out;
            for (const auto& rule : rules->children("LoadProfileBusinessDayRule")) {
                if (!out.empty())
                    out += ";";
                out += text_of(rule, "Date") + "|" + text_of(rule, "Calendar") + "|" +
                       serialize_factors(rule.child("BusinessDayLoadFactors")) + "|" +
                       serialize_factors(rule.child("NonBusinessDayLoadFactors"));
            }
            r.business_day_load_rules = out;
        }
    }

    set_audit(r);
    return r;
}

refdata::domain::bond_yield_convention conventions_mapper::map_bond_yield(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping bond yield convention: " << text_of(v, "Id");

    refdata::domain::bond_yield_convention r;
    r.id = text_of(v, "Id");
    r.compounding = text_of(v, "Compounding");
    assign(r.frequency, opt_frequency(v, "Frequency"));
    assign(r.price_type, opt_text(v, "PriceType"));
    assign(r.accuracy, opt_real(v, "Accuracy"));
    assign(r.max_evaluations, opt_integer(v, "MaxEvaluations"));
    assign(r.guess, opt_real(v, "Guess"));
    set_audit(r);
    return r;
}

namespace {

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

    assign(r.option_expiry_last_weekday_of_month, opt_spelling(v, "OptionExpiryLastWeekdayOfMonth"));
    assign(r.option_expiry_weekly_day_of_the_week, opt_spelling(v, "OptionExpiryWeeklyDayOfTheWeek"));
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
    assign(r.balance_of_the_month_pricing_calendar, opt_text(v, "BalanceOfTheMonthPricingCalendar"));
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

refdata::domain::cms_spread_option_convention
conventions_mapper::map_cms_spread_option(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping CMS spread option convention: " << text_of(v, "Id");

    refdata::domain::cms_spread_option_convention r;
    r.id = text_of(v, "Id");
    r.forward_start = text_of(v, "ForwardStart");
    r.spot_days = text_of(v, "SpotDays");
    r.swap_tenor = text_of(v, "SwapTenor");
    r.fixing_days = integer_of(v, "FixingDays");
    r.calendar = text_of(v, "Calendar");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    r.roll_convention = bdc_of(v, "RollConvention");

    set_audit(r);
    return r;
}

refdata::domain::cross_currency_fix_float_convention
conventions_mapper::map_cross_currency_fix_float(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency fix-float convention: "
                               << text_of(v, "Id");

    refdata::domain::cross_currency_fix_float_convention r;
    r.id = text_of(v, "Id");
    r.settlement_days = integer_of(v, "SettlementDays");
    r.settlement_calendar = text_of(v, "SettlementCalendar");
    r.settlement_convention = bdc_of(v, "SettlementConvention");
    r.fixed_currency = spelling_of(v, "FixedCurrency", "currency code");
    r.fixed_frequency = frequency_of(v, "FixedFrequency");
    r.fixed_convention = bdc_of(v, "FixedConvention");
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    r.index = text_of(v, "Index");
    assign(r.eom, opt_flag(v, "EOM"));
    assign(r.is_resettable, opt_flag(v, "IsResettable"));
    assign(r.float_index_is_resettable, opt_flag(v, "FloatIndexIsResettable"));
    assign(r.include_spread, opt_flag(v, "IncludeSpread"));
    assign(r.lookback, opt_text(v, "Lookback"));
    assign(r.fixing_days, opt_integer(v, "FixingDays"));
    assign(r.rate_cutoff, opt_integer(v, "RateCutoff"));
    assign(r.is_averaged, opt_flag(v, "IsAveraged"));
    assign(r.observation_shift, opt_flag(v, "ObservationShift"));
    set_audit(r);
    return r;
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

refdata::domain::bma_basis_swap_convention
conventions_mapper::map_bma_basis_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping BMA basis swap convention: " << text_of(v, "Id");

    refdata::domain::bma_basis_swap_convention r;
    r.id = text_of(v, "Id");
    r.index = text_of(v, "Index");
    r.bma_index = text_of(v, "BMAIndex");
    assign(r.bma_payment_calendar, opt_text(v, "BMAPaymentCalendar"));
    assign(r.bma_payment_convention, opt_bdc(v, "BMAPaymentConvention"));
    assign(r.bma_payment_lag, opt_integer(v, "BMAPaymentLag"));
    assign(r.index_payment_calendar, opt_text(v, "IndexPaymentCalendar"));
    assign(r.index_payment_convention, opt_bdc(v, "IndexPaymentConvention"));
    assign(r.index_payment_lag, opt_integer(v, "IndexPaymentLag"));
    assign(r.index_settlement_days, opt_integer(v, "IndexSettlementDays"));
    assign(r.index_payment_period, opt_text(v, "IndexPaymentPeriod"));
    assign(r.overnight_lockout_days, opt_integer(v, "OvernightLockoutDays"));
    set_audit(r);
    return r;
}

refdata::domain::zero_inflation_index_convention
conventions_mapper::map_zero_inflation_index(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping zero inflation index convention: " << text_of(v, "Id");

    refdata::domain::zero_inflation_index_convention r;
    r.id = text_of(v, "Id");
    r.region_name = text_of(v, "RegionName");
    r.region_code = text_of(v, "RegionCode");
    r.revised = flag_of(v, "Revised");
    r.frequency = frequency_of(v, "Frequency");
    r.availability_lag = text_of(v, "AvailabilityLag");
    r.currency = spelling_of(v, "Currency", "currency code");

    set_audit(r);
    return r;
}

refdata::domain::overnight_index_convention
conventions_mapper::map_overnight_index(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping overnight index convention: " << text_of(v, "Id");

    refdata::domain::overnight_index_convention r;
    r.id = text_of(v, "Id");
    r.fixing_calendar = text_of(v, "FixingCalendar");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    r.settlement_days = integer_of(v, "SettlementDays");
    set_audit(r);
    return r;
}

ores::refdata::messaging::fx_convention conventions_mapper::map_fx(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX convention: " << text_of(v, "Id");

    ores::refdata::messaging::fx_convention r;
    r.spot_days = integer_of(v, "SpotDays");
    const double points_factor = decimal_of(v, "PointsFactor");

    auto& pair = r.pair;
    pair.base_currency = spelling_of(v, "SourceCurrency", "currency code");
    pair.quote_currency = spelling_of(v, "TargetCurrency", "currency code");
    pair.pair_code = pair.base_currency + "/" + pair.quote_currency;
    set_audit(pair);

    auto& convention = r.convention;
    convention.pair_code = pair.pair_code;
    convention.pip_factor = points_factor != 0.0 ? 1.0 / points_factor : 0.0;
    convention.tick_size = 1.0;
    convention.decimal_places =
        points_factor > 0.0 ? static_cast<int>(std::lround(std::log10(points_factor))) : 0;

    if (const auto joined = opt_text(v, "AdvanceCalendar")) {
        std::size_t start = 0;
        while (start <= joined->size()) {
            const auto comma = joined->find(',', start);
            const auto end = comma == std::string::npos ? joined->size() : comma;
            if (end > start)
                r.advance_calendars.push_back(joined->substr(start, end - start));
            if (comma == std::string::npos)
                break;
            start = comma + 1;
        }
    }

    assign(convention.spot_relative, opt_flag(v, "SpotRelative"));
    assign(convention.end_of_month, opt_flag(v, "EOM"));
    assign(convention.business_day_convention, opt_bdc(v, "Convention"));

    set_audit(convention);
    return r;
}

refdata::domain::cds_convention conventions_mapper::map_cds(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping CDS convention: " << text_of(v, "Id");

    refdata::domain::cds_convention r;
    r.id = text_of(v, "Id");
    assign(r.settlement_days, opt_integer(v, "SettlementDays"));
    assign(r.calendar, opt_text(v, "Calendar"));
    assign(r.frequency, opt_frequency(v, "Frequency"));
    assign(r.payment_convention, opt_bdc(v, "PaymentConvention"));
    assign(r.rule, opt_date_rule(v, "Rule"));
    assign(r.day_count_fraction, opt_day_counter(v, "DayCounter"));
    assign(r.upfront_settlement_days, opt_integer(v, "UpfrontSettlementDays"));
    r.settles_accrual = flag_of(v, "SettlesAccrual");
    r.pays_at_default_time = flag_of(v, "PaysAtDefaultTime");
    assign(r.last_period_day_count_fraction, opt_day_counter(v, "LastPeriodDayCounter"));

    set_audit(r);
    return r;
}

// ---------------------------------------------------------------------------
// Top-level mapper
// ---------------------------------------------------------------------------

conventions conventions_mapper::read(std::string_view text, std::string_view origin) {
    conventions record;
    const auto bound = xml::binder::bind(xg::conventions_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("conventions") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string conventions_mapper::write(const conventions& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(xg::conventions_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

std::map<std::string, std::size_t> conventions_mapper::unmodelled(const conventions& v) {
    // Every category the document carries that this mapper does not model. A
    // skip that is counted is a gap a caller can read; a skip that is silent is
    // a document losing content and saying nothing.
    std::map<std::string, std::size_t> r;
    const auto count = [&r](std::string_view name, std::size_t n) {
        if (n != 0)
            r.emplace(std::string(name), n);
    };
    const record_reader root(*xg::conventions_document().root, &v);
    count("FxOptionTimeWeighting", root.children("FxOptionTimeWeighting").size());

    std::size_t rebasing_events = 0;
    for (const auto& x : root.children("ZeroInflationIndex"))
        if (x.has("RebasingEvents"))
            ++rebasing_events;
    count("ZeroInflationIndex.RebasingEvents", rebasing_events);
    return r;
}

// ---------------------------------------------------------------------------
// The package — the document as the families it carries
// ---------------------------------------------------------------------------

namespace {

using package_row = refdata::service::conventions_bundle_row;

/**
 * @brief One family's elements, mapped and packed in document order.
 */
template <typename Convert>
void pack_family(refdata::service::conventions_bundle& bundle,
                 std::string_view family,
                 const std::vector<record_reader>& rows,
                 Convert convert) {
    for (std::size_t i = 0; i < rows.size(); ++i)
        bundle.rows.push_back(
            {std::string(family), static_cast<int>(i), rfl::json::write(convert(rows[i]))});
}

void pack_row(refdata::service::conventions_bundle& bundle,
              std::string_view family,
              int sequence,
              const auto& row) {
    bundle.rows.push_back({std::string(family), sequence, rfl::json::write(row)});
}

/**
 * @brief One payload as the family's own type.
 *
 * A payload that does not decode is a family that cannot be written, so it
 * throws rather than disappearing from the document.
 */
template <typename Row>
Row unpack_row(const package_row& row) {
    auto decoded = rfl::json::read<Row>(row.payload);
    if (!decoded)
        throw std::runtime_error(
            std::format("Conventions payload for family \"{}\" does not decode.", row.family));
    return std::move(*decoded);
}

/**
 * @brief How one family's payload becomes the document's elements.
 *
 * The three tenor basis swap families are absent because they are one document
 * element, and the FX conventions are absent because they are a projection and
 * never enter the bundle.
 */
struct family_joiner {
    std::string_view family;
    void (*join)(const package_row& row, record_writer& root);
};

constexpr family_joiner joiners[] = {
    {"average_ois",
     [](const package_row& row, record_writer& root) {
         reverse_average_ois(unpack_row<refdata::domain::average_ois_convention>(row),
                             root.append("AverageOIS"));
     }},
    {"bma_basis_swap",
     [](const package_row& row, record_writer& root) {
         reverse_bma_basis_swap(unpack_row<refdata::domain::bma_basis_swap_convention>(row),
                                root.append("BMABasisSwap"));
     }},
    {"bond_yield",
     [](const package_row& row, record_writer& root) {
         reverse_bond_yield(unpack_row<refdata::domain::bond_yield_convention>(row),
                            root.append("BondYield"));
     }},
    {"cds",
     [](const package_row& row, record_writer& root) {
         reverse_cds(unpack_row<refdata::domain::cds_convention>(row), root.append("CDS"));
     }},
    {"cms_spread_option",
     [](const package_row& row, record_writer& root) {
         reverse_cms_spread_option(unpack_row<refdata::domain::cms_spread_option_convention>(row),
                                   root.append("CmsSpreadOption"));
     }},
    {"commodity_forward",
     [](const package_row& row, record_writer& root) {
         reverse_commodity_forward(unpack_row<refdata::domain::commodity_forward_convention>(row),
                                   root.append("CommodityForward"));
     }},
    {"commodity_future",
     [](const package_row& row, record_writer& root) {
         reverse_commodity_future(unpack_row<refdata::domain::commodity_future_convention>(row),
                                  root.append("CommodityFuture"));
     }},
    {"cross_currency_basis",
     [](const package_row& row, record_writer& root) {
         reverse_cross_currency_basis(
             unpack_row<refdata::domain::cross_currency_basis_convention>(row),
             root.append("CrossCurrencyBasis"));
     }},
    {"cross_currency_fix_float",
     [](const package_row& row, record_writer& root) {
         reverse_cross_currency_fix_float(
             unpack_row<refdata::domain::cross_currency_fix_float_convention>(row),
             root.append("CrossCurrencyFixFloat"));
     }},
    {"deposit",
     [](const package_row& row, record_writer& root) {
         reverse_deposit(unpack_row<refdata::domain::deposit_convention>(row),
                         root.append("Deposit"));
     }},
    {"fra",
     [](const package_row& row, record_writer& root) {
         reverse_fra(unpack_row<refdata::domain::fra_convention>(row), root.append("FRA"));
     }},
    {"future",
     [](const package_row& row, record_writer& root) {
         reverse_future(unpack_row<refdata::domain::future_convention>(row),
                        root.append("Future"));
     }},
    {"fx_option",
     [](const package_row& row, record_writer& root) {
         reverse_fx_option(unpack_row<refdata::domain::fx_option_convention>(row),
                           root.append("FxOption"));
     }},
    {"ibor_index",
     [](const package_row& row, record_writer& root) {
         reverse_ibor_index(unpack_row<refdata::domain::ibor_index_convention>(row),
                            root.append("IborIndex"));
     }},
    {"inflation_swap",
     [](const package_row& row, record_writer& root) {
         reverse_inflation_swap(unpack_row<refdata::domain::inflation_swap_convention>(row),
                                root.append("InflationSwap"));
     }},
    {"intraday_power_load",
     [](const package_row& row, record_writer& root) {
         reverse_intraday_power_load(
             unpack_row<refdata::domain::intraday_power_load_convention>(row),
             root.append("IntradayPowerLoad"));
     }},
    {"ois",
     [](const package_row& row, record_writer& root) {
         reverse_ois(unpack_row<refdata::domain::ois_convention>(row), root.append("OIS"));
     }},
    {"overnight_index",
     [](const package_row& row, record_writer& root) {
         reverse_overnight_index(unpack_row<refdata::domain::overnight_index_convention>(row),
                                 root.append("OvernightIndex"));
     }},
    {"swap",
     [](const package_row& row, record_writer& root) {
         reverse_swap(unpack_row<refdata::domain::swap_convention>(row), root.append("Swap"));
     }},
    {"swap_index",
     [](const package_row& row, record_writer& root) {
         reverse_swap_index(unpack_row<refdata::domain::swap_index_convention>(row),
                            root.append("SwapIndex"));
     }},
    {"tenor_basis_two_swap",
     [](const package_row& row, record_writer& root) {
         reverse_tenor_basis_two_swap(
             unpack_row<refdata::domain::tenor_basis_two_swap_convention>(row),
             root.append("TenorBasisTwoSwap"));
     }},
    {"zero",
     [](const package_row& row, record_writer& root) {
         reverse_zero(unpack_row<refdata::domain::zero_convention>(row), root.append("Zero"));
     }},
    {"zero_inflation_index",
     [](const package_row& row, record_writer& root) {
         reverse_zero_inflation_index(
             unpack_row<refdata::domain::zero_inflation_index_convention>(row),
             root.append("ZeroInflationIndex"));
     }},
};

}

conventions_package split_conventions(const conventions& v) {
    conventions_package p;
    const record_reader root(*xg::conventions_document().root, &v);

    pack_family(
        p.bundle, "average_ois", root.children("AverageOIS"), &conventions_mapper::map_average_ois);
    pack_family(p.bundle,
                "bma_basis_swap",
                root.children("BMABasisSwap"),
                &conventions_mapper::map_bma_basis_swap);
    pack_family(
        p.bundle, "bond_yield", root.children("BondYield"), &conventions_mapper::map_bond_yield);
    pack_family(p.bundle, "cds", root.children("CDS"), &conventions_mapper::map_cds);
    pack_family(p.bundle,
                "cms_spread_option",
                root.children("CmsSpreadOption"),
                &conventions_mapper::map_cms_spread_option);
    pack_family(p.bundle,
                "commodity_forward",
                root.children("CommodityForward"),
                &conventions_mapper::map_commodity_forward);
    pack_family(p.bundle,
                "commodity_future",
                root.children("CommodityFuture"),
                &conventions_mapper::map_commodity_future);

    // A tenor basis swap is one central convention row plus the parts the
    // document's form populated, and the three are three families.
    int conventions_seen = 0;
    int pay_receives_seen = 0;
    int long_shorts_seen = 0;
    for (const auto& x : root.children("TenorBasisSwap")) {
        auto item = conventions_mapper::map_tenor_basis_swap(x);
        if (item.convention)
            pack_row(p.bundle, "convention", conventions_seen++, *item.convention);
        for (const auto& part : item.pay_receive)
            pack_row(p.bundle, "tenor_basis_swap_pay_receive", pay_receives_seen++, part);
        for (const auto& part : item.long_short)
            pack_row(p.bundle, "tenor_basis_swap_long_short", long_shorts_seen++, part);
    }

    pack_family(p.bundle,
                "cross_currency_basis",
                root.children("CrossCurrencyBasis"),
                &conventions_mapper::map_cross_currency_basis);
    pack_family(p.bundle,
                "cross_currency_fix_float",
                root.children("CrossCurrencyFixFloat"),
                &conventions_mapper::map_cross_currency_fix_float);
    pack_family(p.bundle, "deposit", root.children("Deposit"), &conventions_mapper::map_deposit);
    pack_family(p.bundle, "fra", root.children("FRA"), &conventions_mapper::map_fra);
    pack_family(p.bundle, "future", root.children("Future"), &conventions_mapper::map_future);
    pack_family(
        p.bundle, "fx_option", root.children("FxOption"), &conventions_mapper::map_fx_option);
    pack_family(
        p.bundle, "ibor_index", root.children("IborIndex"), &conventions_mapper::map_ibor_index);
    pack_family(p.bundle,
                "inflation_swap",
                root.children("InflationSwap"),
                &conventions_mapper::map_inflation_swap);
    pack_family(p.bundle,
                "intraday_power_load",
                root.children("IntradayPowerLoad"),
                &conventions_mapper::map_intraday_power_load);
    pack_family(p.bundle, "ois", root.children("OIS"), &conventions_mapper::map_ois);
    pack_family(p.bundle,
                "overnight_index",
                root.children("OvernightIndex"),
                &conventions_mapper::map_overnight_index);
    pack_family(p.bundle, "swap", root.children("Swap"), &conventions_mapper::map_swap);
    pack_family(
        p.bundle, "swap_index", root.children("SwapIndex"), &conventions_mapper::map_swap_index);
    pack_family(p.bundle,
                "tenor_basis_two_swap",
                root.children("TenorBasisTwoSwap"),
                &conventions_mapper::map_tenor_basis_two_swap);
    pack_family(p.bundle, "zero", root.children("Zero"), &conventions_mapper::map_zero);
    pack_family(p.bundle,
                "zero_inflation_index",
                root.children("ZeroInflationIndex"),
                &conventions_mapper::map_zero_inflation_index);

    // An FX convention is a projection of a currency pair, that pair's
    // convention and its advance calendars, so it is kept aside and never
    // packed into a family of its own.
    const auto fx = root.children("FX");
    p.fx.reserve(fx.size());
    for (const auto& x : fx)
        p.fx.push_back(conventions_mapper::map_fx(x));

    return p;
}

conventions join_conventions(const conventions_package& p) {
    conventions r;
    record_writer root(*xg::conventions_document().root, &r);

    std::vector<refdata::domain::convention> central;
    std::vector<refdata::domain::tenor_basis_swap_pay_receive> pay_receives;
    std::vector<refdata::domain::tenor_basis_swap_long_short> long_shorts;

    for (const auto& row : p.bundle.rows) {
        // The one table carries every kind, and only this kind's rows have a
        // document element. A row of another kind is a convention with no
        // element to be written as, so it is dropped rather than written as a
        // tenor basis swap that carries no legs.
        if (row.family == "convention") {
            auto c = unpack_row<refdata::domain::convention>(row);
            if (c.kind_code == tenor_basis_swap_kind)
                central.push_back(std::move(c));
            continue;
        }
        if (row.family == "tenor_basis_swap_pay_receive") {
            pay_receives.push_back(unpack_row<refdata::domain::tenor_basis_swap_pay_receive>(row));
            continue;
        }
        if (row.family == "tenor_basis_swap_long_short") {
            long_shorts.push_back(unpack_row<refdata::domain::tenor_basis_swap_long_short>(row));
            continue;
        }

        const auto joiner = std::ranges::find(joiners, row.family, &family_joiner::family);
        if (joiner == std::end(joiners))
            throw std::runtime_error(std::format(
                "Conventions payload names family \"{}\", which no ORE document element carries.",
                row.family));
        joiner->join(row, root);
    }

    // The three families are one document element, so a central row takes back
    // the parts that name it. The row order is the bundle's order; the sequence
    // each row carries orders a family within a bundle, and the join does not
    // need it because a family's rows are appended in the order they arrive.
    for (const auto& c : central) {
        refdata::messaging::tenor_basis_swap_convention_item item;
        item.key.id = c.id;
        item.convention = c;
        for (const auto& part : pay_receives)
            if (part.convention_id == c.id)
                item.pay_receive.push_back(part);
        for (const auto& part : long_shorts)
            if (part.convention_id == c.id)
                item.long_short.push_back(part);
        reverse_tenor_basis_swap(item, root.append("TenorBasisSwap"));
    }

    for (const auto& fx : p.fx)
        reverse_fx(fx, root.append("FX"));

    return r;
}

std::vector<std::string> skipped_fx_conventions(const conventions_package& p) {
    std::vector<std::string> names;
    names.reserve(p.fx.size());
    for (const auto& fx : p.fx)
        names.push_back(fx_convention_id(fx.pair.base_currency, fx.pair.quote_currency));
    return names;
}

}
