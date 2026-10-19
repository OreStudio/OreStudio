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
#include "conventions_fields.hpp"
#include <format>
#include <sstream>
#include <stdexcept>

namespace ores::ore::domain::conventions_fields {

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

double decimal_of(const record_reader& v, std::string_view name) {
    if (const auto x = v.decimal(name))
        return x->to_double();
    return 0.0;
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

bool is_true(std::string_view text) {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
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

std::string text_flag_field(const record_reader& v, std::string_view name) {
    const auto x = v.text(name);
    return x ? truth(is_true(*x)) : std::string();
}

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

void put_enum(record_writer& w,
              std::string_view name,
              const std::string& value,
              std::string_view what) {
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
        throw std::runtime_error(
            std::format("'{}' is not an exact decimal: {}", name, exact.error()));
    w.set_decimal(name, *exact);
}
void put_decimal(record_writer& w, std::string_view name, const std::optional<double>& value) {
    if (value)
        put_decimal(w, name, *value);
}

}
