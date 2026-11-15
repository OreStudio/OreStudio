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

/// A family, as the shape table keys it: the asset class, the subclass, the index family.
using family_key = std::tuple<std::string, std::string, std::string>;

constexpr std::string_view required_says = "required";
constexpr std::string_view forbidden_says = "forbidden";
constexpr std::string_view kind_index = "index";
constexpr std::string_view index_subclass = "index_fixing";

/// The fields one family requires and the ones it forbids.
struct family_shape final {
    std::vector<std::string> required;
    std::vector<std::string> forbidden;
};

family_key key_of(const market_series_identity& row, const market_series& series) {
    return family_key{row.asset_class, series.series_subclass, row.index};
}

std::map<family_key, family_shape> book_of(const std::vector<series_identity_shape>& stated) {
    std::map<family_key, family_shape> book;
    for (const auto& row : stated) {
        auto& shape = book[family_key{row.asset_class, row.series_subclass, row.index_family}];
        if (row.requirement == required_says)
            shape.required.push_back(row.field);
        else if (row.requirement == forbidden_says)
            shape.forbidden.push_back(row.field);
    }
    return book;
}

std::string family_name(const market_series_identity& row, const market_series& series) {
    return row.asset_class + "/" + series.series_subclass + "/" + row.index;
}

}

std::string text_of(const market_series_identity& row, std::string_view column) {
    std::string out;
    ores::utility::codec::apply_fields(
        [&](const auto&... one) {
            ([&] {
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
    if (row.identity_kind != kind_index || series.series_subclass == index_subclass)
        return;
    throw std::invalid_argument("series '" + series.oresmd_uri + "' is a fixing of the index '" +
                                row.index + "', so its series subclass is '" +
                                std::string(index_subclass) + "' and not '" +
                                series.series_subclass + "'");
}

void check_identity_shape(const std::vector<series_identity_shape>& stated,
                          const market_series& series,
                          const market_series_identity& row) {
    const auto book = book_of(stated);
    const auto shape = book.find(key_of(row, series));
    if (shape == book.end())
        return;
    for (const auto& field : shape->second.required) {
        if (text_of(row, field).empty())
            throw std::invalid_argument("series '" + series.oresmd_uri + "' is not a " +
                                        family_name(row, series) + " identity: field '" + field +
                                        "' is required");
    }
    for (const auto& field : shape->second.forbidden) {
        if (!text_of(row, field).empty())
            throw std::invalid_argument("series '" + series.oresmd_uri + "' is not a " +
                                        family_name(row, series) + " identity: field '" + field +
                                        "' is forbidden");
    }
}

bool states_identity_shape(const std::vector<series_identity_shape>& stated,
                           const market_series& series,
                           const market_series_identity& row) {
    const auto book = book_of(stated);
    return book.contains(key_of(row, series));
}

}
