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
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.ore.core/results/output_catalogue_data.hpp"
#include <algorithm>
#include <stdexcept>
#include <string>

namespace ores::ore::results {
namespace {

std::vector<std::string> split(std::string_view text, char separator) {
    std::vector<std::string> fields;
    std::string field;
    for (const auto c : text) {
        if (c == separator) {
            fields.push_back(field);
            field.clear();
        } else {
            field.push_back(c);
        }
    }
    fields.push_back(field);
    return fields;
}

std::vector<std::string> split_list(std::string_view text) {
    std::vector<std::string> names;
    for (auto&& name : split(text, ',')) {
        if (!name.empty())
            names.push_back(std::move(name));
    }
    return names;
}

output_column_kind kind_named(std::string_view kind) {
    if (kind == "integer")
        return output_column_kind::integer;
    if (kind == "decimal")
        return output_column_kind::decimal;
    if (kind == "date")
        return output_column_kind::date;
    if (kind == "time")
        return output_column_kind::time;
    if (kind == "text")
        return output_column_kind::text;
    throw std::invalid_argument("the catalogue states an unknown column kind: '" +
                                std::string(kind) + "'.");
}

output_family parse(std::string_view line) {
    const auto fields = split(line, '\t');
    if (fields.size() != 12)
        throw std::invalid_argument("a catalogue line states " + std::to_string(fields.size()) +
                                    " fields, not twelve: '" + std::string(line) + "'.");

    output_family family;
    family.code = fields[0];
    family.file_pattern = fields[1];
    family.file_parameter = fields[2];
    family.analytic_codes = split_list(fields[3]);
    family.key_columns = split_list(fields[4]);
    family.required_columns = split_list(fields[5]);
    family.qualifier_columns = split_list(fields[6]);
    family.measure_columns = split_list(fields[7]);
    family.measures_open = fields[8] == "1";
    family.header_variants = std::stoi(fields[9]);
    family.sample_file = fields[10];

    for (const auto& text : split(fields[11], ',')) {
        if (text.empty())
            continue;
        const auto colon = text.rfind(':');
        if (colon == std::string::npos)
            throw std::invalid_argument("a catalogue column states no kind: '" + text + "'.");
        family.columns.push_back(output_column{.name = text.substr(0, colon),
                                               .kind = kind_named(text.substr(colon + 1))});
    }
    return family;
}

std::vector<output_family> parse_catalogue() {
    std::vector<output_family> families;
    std::string_view text = output_catalogue_text;
    std::size_t offset = 0;
    while (offset < text.size()) {
        const auto line_break = text.find('\n', offset);
        const auto line = text.substr(offset,
                                      line_break == std::string_view::npos ? text.size() - offset :
                                                                             line_break - offset);
        offset = line_break == std::string_view::npos ? text.size() : line_break + 1;
        if (line.empty())
            continue;
        families.push_back(parse(line));
    }
    return families;
}

bool pattern_matches(std::string_view pattern, std::string_view file_name) {
    const auto star = pattern.find('*');
    if (star == std::string_view::npos)
        return pattern == file_name;
    const auto prefix = pattern.substr(0, star);
    const auto suffix = pattern.substr(star + 1);
    return file_name.size() >= prefix.size() + suffix.size() && file_name.starts_with(prefix) &&
           file_name.ends_with(suffix);
}

}

const std::vector<output_family>& output_families() {
    static const std::vector<output_family> families = parse_catalogue();
    return families;
}

const output_family* family_named(std::string_view code) {
    const auto& families = output_families();
    const auto it = std::ranges::find_if(
        families, [code](const output_family& family) { return family.code == code; });
    return it == families.end() ? nullptr : &*it;
}

const output_family* family_of_file(std::string_view file_name) {
    const auto& families = output_families();
    const auto it = std::ranges::find_if(families, [file_name](const output_family& family) {
        return pattern_matches(family.file_pattern, file_name);
    });
    return it == families.end() ? nullptr : &*it;
}

const output_column* column_named(const output_family& family, std::string_view name) {
    const auto it = std::ranges::find_if(
        family.columns, [name](const output_column& column) { return column.name == name; });
    return it == family.columns.end() ? nullptr : &*it;
}

output_column_role role_of(const output_family& family, std::string_view name) {
    const auto holds = [name](const std::vector<std::string>& names) {
        return std::ranges::find(names, name) != names.end();
    };
    if (holds(family.key_columns))
        return output_column_role::key;
    if (holds(family.qualifier_columns))
        return output_column_role::qualifier;
    if (holds(family.measure_columns))
        return output_column_role::measure;
    return output_column_role::none;
}

std::string_view to_string(output_column_kind kind) {
    switch (kind) {
        case output_column_kind::integer:
            return "integer";
        case output_column_kind::decimal:
            return "decimal";
        case output_column_kind::date:
            return "date";
        case output_column_kind::time:
            return "time";
        case output_column_kind::text:
            break;
    }
    return "text";
}

}
