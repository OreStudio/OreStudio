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
#include "ores.ore.core/market/series_key_registry.hpp"
#include <algorithm>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::ore::market {

namespace {

std::vector<std::string_view> split_slash(std::string_view s) {
    std::vector<std::string_view> parts;
    std::size_t start = 0;
    while (true) {
        const auto pos = s.find('/', start);
        if (pos == std::string_view::npos) {
            parts.push_back(s.substr(start));
            break;
        }
        parts.push_back(s.substr(start, pos - start));
        start = pos + 1;
    }
    return parts;
}

std::string
join_slash(const std::vector<std::string_view>& parts, std::size_t start, std::size_t end) {
    std::string result;
    for (auto i = start; i < end; ++i) {
        if (i > start)
            result += '/';
        result.append(parts[i]);
    }
    return result;
}

} // namespace

series_key_registry::series_key_registry(std::vector<domain::series_key_shape> shapes) {
    if (shapes.empty())
        throw std::invalid_argument(
            "ores_ore_series_key_shapes_tbl is unseeded: no series key shapes were supplied, "
            "so every key would fold into its qualifier and each distinct key would become a "
            "series of its own.");

    for (auto& shape : shapes) {
        if (shape.has_point_dimension && !shape.default_point.empty())
            throw std::invalid_argument(
                "series key shape for type '" + shape.series_type +
                "' claims a point dimension and also carries the default point '" +
                shape.default_point +
                "'; a type whose keys carry a point has no need of a default, and a reader "
                "would not know which of the two to record.");

        by_type_.insert_or_assign(shape.series_type, std::move(shape));
    }
}

decomposed_key series_key_registry::decompose(const std::string& key) const {
    const auto parts = split_slash(key);
    if (parts.size() < 2)
        throw std::invalid_argument("ORE key has fewer than 2 segments: " + key);

    decomposed_key dk;
    dk.series_type = std::string(parts[0]);
    dk.metric = std::string(parts[1]);

    const auto it = by_type_.find(dk.series_type);

    if (it == by_type_.end()) {
        // Unknown type — absorb all remaining segments into qualifier, no point_id.
        // This guarantees a lossless roundtrip for any new or non-standard key type.
        dk.qualifier = join_slash(parts, 2, parts.size());
        return dk;
    }

    const auto& shape = it->second;
    const std::size_t q_end = 2 + static_cast<std::size_t>(shape.qualifier_depth);

    if (parts.size() <= q_end || !shape.has_point_dimension) {
        // All remaining segments form the qualifier: the type has no point
        // dimension, or the key is shorter than the type expects.
        dk.qualifier = join_slash(parts, 2, parts.size());
        return dk;
    }

    dk.qualifier = join_slash(parts, 2, q_end);
    dk.point_id = join_slash(parts, q_end, parts.size());
    return dk;
}

bool series_key_registry::has_point_dimension(const std::string& series_type) const {
    const auto it = by_type_.find(series_type);
    return it != by_type_.end() && it->second.has_point_dimension;
}

std::string series_key_registry::default_point_for(const std::string& series_type) const {
    const auto it = by_type_.find(series_type);
    return it == by_type_.end() ? std::string{} : it->second.default_point;
}

std::vector<std::string> series_key_registry::known_series_types() const {
    std::vector<std::string> types;
    types.reserve(by_type_.size());
    for (const auto& [series_type, shape] : by_type_)
        types.push_back(series_type);
    std::sort(types.begin(), types.end());
    return types;
}

std::string reconstruct_key(const decomposed_key& dk) {
    auto key = dk.series_type + '/' + dk.metric + '/' + dk.qualifier;
    if (dk.point_id)
        key += '/' + *dk.point_id;
    return key;
}

} // namespace ores::ore::market
