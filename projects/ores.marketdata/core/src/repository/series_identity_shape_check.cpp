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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.core/repository/series_identity_shape_check.hpp"
#include "ores.marketdata.api/domain/market_series_identity_codec.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <algorithm>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::marketdata::repository {

namespace {

using domain::market_series;
using domain::market_series_identity;
using domain::series_identity_shape;
using domain::series_identity_shape_entry;

/// A family, as the shape table keys it: the asset class, the subclass, the index family.
using family_key = std::tuple<std::string, std::string, std::string>;

constexpr std::string_view required_says = "required";
constexpr std::string_view forbidden_says = "forbidden";
constexpr std::string_view index_series_kind = "index";
constexpr std::string_view index_subclass = "index_fixing";

family_key family_key_of(const market_series_identity& row, const market_series& series) {
    return family_key{row.asset_class, series.series_subclass, row.index};
}

std::string family_name(const market_series_identity& row, const market_series& series) {
    return row.asset_class + "/" + series.series_subclass + "/" + row.index;
}

}

std::optional<std::string> text_of(const market_series_identity& row, std::string_view column) {
    std::optional<std::string> out;
    ores::utility::codec::apply_fields(
        [&](const auto&... one) {
            (
                [&] {
                    if (one.name != column)
                        return;
                    using member_type = std::remove_cvref_t<decltype(row.*(one.member))>;
                    if constexpr (std::is_same_v<member_type, std::string>)
                        out = row.*(one.member);
                }(),
                ...);
        },
        wire_fields(std::type_identity<market_series_identity>{}));
    return out;
}

void check_identity_kind(const market_series& series, const market_series_identity& row) {
    if (row.identity_kind != index_series_kind || series.series_subclass == index_subclass)
        return;
    throw std::invalid_argument("series '" + series.oresmd_uri + "' is a fixing of the index '" +
                                row.index + "', so its series subclass is '" +
                                std::string(index_subclass) + "' and not '" +
                                series.series_subclass + "'");
}

identity_shape_book::identity_shape_book(const std::vector<series_identity_shape>& shapes,
                                         const std::vector<series_identity_shape_entry>& entries) {
    std::map<std::string, family_shape*, std::less<>> by_code;
    for (const auto& shape : shapes) {
        by_code[shape.code] =
            &families_[family_key{shape.asset_class, shape.series_subclass, shape.index_family}];
    }
    for (const auto& entry : entries) {
        const auto shape = by_code.find(entry.shape_code);
        if (shape == by_code.end())
            throw std::logic_error("an entry for field '" + entry.field + "' names shape '" +
                                   entry.shape_code + "', which no stated shape has");
        if (entry.requirement == required_says)
            shape->second->required.push_back(entry.field);
        else if (entry.requirement == forbidden_says)
            shape->second->forbidden.push_back(entry.field);
    }
}

void identity_shape_book::check(const market_series& series,
                                const market_series_identity& row) const {
    const auto shape = families_.find(family_key_of(row, series));
    if (shape == families_.end())
        return;
    // A row that names a column the identity does not hold states nothing the
    // guard can check. Ignoring it would let a misspelt forbidden field pass every
    // row, so it is refused as a fault of the catalogue, not of the series.
    const auto held = [&](const std::string& field) {
        const auto text = text_of(row, field);
        if (!text)
            throw std::logic_error("the " + family_name(row, series) + " shape names field '" +
                                   field + "', which an identity does not hold as text");
        return *text;
    };
    for (const auto& field : shape->second.required) {
        if (held(field).empty())
            throw std::invalid_argument("series '" + series.oresmd_uri + "' is not a " +
                                        family_name(row, series) + " identity: field '" + field +
                                        "' is required");
    }
    for (const auto& field : shape->second.forbidden) {
        if (!held(field).empty())
            throw std::invalid_argument("series '" + series.oresmd_uri + "' is not a " +
                                        family_name(row, series) + " identity: field '" + field +
                                        "' is forbidden");
    }
}

bool identity_shape_book::states(const market_series& series,
                                 const market_series_identity& row) const {
    return families_.contains(family_key_of(row, series));
}

void check_identity_shape(const std::vector<series_identity_shape>& shapes,
                          const std::vector<series_identity_shape_entry>& entries,
                          const market_series& series,
                          const market_series_identity& row) {
    identity_shape_book(shapes, entries).check(series, row);
}

bool states_identity_shape(const std::vector<series_identity_shape>& shapes,
                           const std::vector<series_identity_shape_entry>& entries,
                           const market_series& series,
                           const market_series_identity& row) {
    return identity_shape_book(shapes, entries).states(series, row);
}

}
