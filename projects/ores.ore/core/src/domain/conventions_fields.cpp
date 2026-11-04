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
#include "conventions_renderings.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <format>
#include <sstream>
#include <stdexcept>

namespace ores::ore::domain::conventions_fields {

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

void put_bdc(record_writer& w, std::string_view name, const std::string& code) {
    w.set_spelling(name, code, "business day convention");
}
void put_bdc(record_writer& w, std::string_view name, const std::optional<std::string>& code) {
    if (code)
        put_bdc(w, name, *code);
}

void put_day_counter(record_writer& w, std::string_view name, const std::string& code) {
    w.set_spelling(name, code, "day counter");
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

}
