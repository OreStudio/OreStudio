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
#include "ores.ore.core/results/payload_index.hpp"
#include "output_csv.hpp"
#include <algorithm>
#include <string>

namespace ores::ore::results {
namespace {

using namespace ores::ore::results::output_csv;

// Whether a key already holds a value, without reading the whole list for every
// row of a file that can hold millions.
bool holds(const std::vector<std::string>& values, const std::string& value) {
    return std::ranges::find(values, value) != values.end();
}

}

payload_index
index_payload(std::string_view csv, const output_family& family, std::vector<std::string>& refusals) {
    payload_index index;

    if (csv.substr(0, 3) == "\xEF\xBB\xBF")
        csv.remove_prefix(3);
    if (csv.empty()) {
        refusals.push_back(family.code + " is empty.");
        return index;
    }

    const auto first_break = csv.find('\n');
    auto header_line = csv.substr(0, first_break);
    if (!header_line.empty() && header_line.front() == '#')
        header_line.remove_prefix(1);
    if (!header_line.empty() && header_line.back() == '\r')
        header_line.remove_suffix(1);
    index.columns = fields_of(header_line);
    if (index.columns.empty() || index.columns.front().empty()) {
        refusals.push_back(family.code + " states no heading.");
        return index;
    }

    // The index records the distinct values of each key the family states, in
    // the order the catalogue names them, so two runs of the same family are
    // read the same way.
    std::vector<std::size_t> key_at;
    for (const auto& key : family.key_columns) {
        const auto at = std::ranges::find(index.columns, key);
        if (at == index.columns.end())
            continue;
        key_at.push_back(static_cast<std::size_t>(std::distance(index.columns.begin(), at)));
        index.keys.emplace_back(key, std::vector<std::string>{});
    }

    std::size_t offset = first_break == std::string_view::npos ? csv.size() : first_break + 1;
    while (offset < csv.size()) {
        const auto line_break = csv.find('\n', offset);
        const auto line = csv.substr(offset,
                                     line_break == std::string_view::npos ? csv.size() - offset :
                                                                            line_break - offset);
        offset = line_break == std::string_view::npos ? csv.size() : line_break + 1;
        if (line.find_first_not_of("\r \t") == std::string_view::npos)
            continue;
        ++index.rows;

        const auto fields = fields_of(line);
        for (std::size_t i = 0; i < key_at.size(); ++i) {
            const auto at = key_at[i];
            if (at >= fields.size())
                continue;
            const auto& value = fields[at];
            if (is_null(value))
                continue;
            auto& values = index.keys[i].second;
            if (holds(values, value))
                continue;
            if (values.size() >= payload_index_value_limit) {
                index.truncated = true;
                continue;
            }
            values.push_back(value);
        }
    }

    return index;
}

payload_index index_payload_for_file(std::string_view csv,
                                     std::string_view file_name,
                                     std::vector<std::string>& refusals) {
    const auto* family = family_of_file(file_name);
    if (family == nullptr) {
        refusals.push_back("no family states the file " + std::string(file_name) + ".");
        return {};
    }
    return index_payload(csv, *family, refusals);
}

}
