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
#ifndef ORES_ORE_CORE_DOMAIN_CONVENTIONS_FIELDS_HPP
#define ORES_ORE_CORE_DOMAIN_CONVENTIONS_FIELDS_HPP

#include "ores.ore.core/xml/record.hpp"
#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file conventions_fields.hpp
 * @brief The field level reading and writing every convention family shares.
 *
 * A family's unit reads its members of the codec's record with these and
 * writes them back with the matching put functions. They are declared in a
 * named namespace and defined once, so no unit holds a private copy and no two
 * units collide when the sources are built as one.
 */
namespace ores::ore::domain::conventions_fields {

using xml::record_reader;
using xml::record_writer;

// Each table maps every spelling the schema allows for an enumeration to the
// canonical code the refdata columns hold.
using code_table = std::map<std::string, std::string, std::less<>>;

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

// A grouped row keeps its audit columns in the group that carries them; a flat
// row is its own. The two differ only in the path, so the fields are named once.
template <typename Row>
auto& audit_of(Row& r) {
    if constexpr (requires { r.audit; })
        return r.audit;
    else
        return r;
}

template <typename T>
void set_audit(T& r) {
    auto& audit = audit_of(r);
    audit.modified_by = std::string(audit_modified_by);
    audit.change_reason_code = std::string(audit_reason_code);
    audit.change_commentary = std::string(audit_commentary);
}

// Assigns to a member that is a value or an optional, only when the document
// states one.
template <typename Target, typename Value>
void assign(Target& target, const std::optional<Value>& value) {
    if (value)
        target = *value;
}

std::string text_of(const record_reader& v, std::string_view name);

std::optional<std::string> opt_text(const record_reader& v, std::string_view name);

int integer_of(const record_reader& v, std::string_view name);

std::optional<int> opt_integer(const record_reader& v, std::string_view name);

bool flag_of(const record_reader& v, std::string_view name);

std::optional<bool> opt_flag(const record_reader& v, std::string_view name);

double decimal_of(const record_reader& v, std::string_view name);

std::string code_of(const code_table& table, std::string_view spelling, std::string_view what);

std::string coded(const record_reader& v,
                  std::string_view name,
                  const code_table& table,
                  std::string_view what);

std::optional<std::string> opt_coded(const record_reader& v,
                                     std::string_view name,
                                     const code_table& table,
                                     std::string_view what);

std::string bdc_of(const record_reader& v, std::string_view name);

std::optional<std::string> opt_bdc(const record_reader& v, std::string_view name);

std::string day_counter_of(const record_reader& v, std::string_view name);

std::optional<std::string> opt_day_counter(const record_reader& v, std::string_view name);

std::string frequency_of(const record_reader& v, std::string_view name);

std::optional<std::string> opt_frequency(const record_reader& v, std::string_view name);

std::optional<std::string> opt_compounding(const record_reader& v, std::string_view name);

std::optional<std::string> opt_date_rule(const record_reader& v, std::string_view name);

// The schema's own spelling of an enumeration whose spelling is also the code.
std::string spelling_of(const record_reader& v, std::string_view name, std::string_view what);

std::optional<std::string> opt_spelling(const record_reader& v, std::string_view name);

bool is_true(std::string_view text);

std::vector<std::string> split_fields(const std::string& s, char sep);

std::string join_with(const std::vector<std::string>& parts, char sep);

std::string truth(bool v);

std::string text_flag_field(const record_reader& v, std::string_view name);

std::optional<std::int64_t> as_integer(const std::optional<int>& v);

std::string bdc_spelling(const std::string& code);

std::string day_counter_spelling(const std::string& code);

void put_bdc(record_writer& w, std::string_view name, const std::string& code);

void put_bdc(record_writer& w, std::string_view name, const std::optional<std::string>& code);

void put_day_counter(record_writer& w, std::string_view name, const std::string& code);

void put_day_counter(record_writer& w,
                     std::string_view name,
                     const std::optional<std::string>& code);

void put_enum(record_writer& w,
              std::string_view name,
              const std::string& value,
              std::string_view what);

void put_enum(record_writer& w,
              std::string_view name,
              const std::optional<std::string>& value,
              std::string_view what);

void put_decimal(record_writer& w, std::string_view name, double value);

void put_decimal(record_writer& w, std::string_view name, const std::optional<double>& value);

}

#endif
