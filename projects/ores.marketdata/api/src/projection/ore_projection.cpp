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
#include "ores.marketdata.api/projection/ore_projection.hpp"
#include "ores.marketdata.api/datum/ore_index_codec.hpp"
#include "ores.marketdata.api/datum/ore_key_codec.hpp"
#include <format>
#include <utility>

namespace ores::marketdata::projection {

ore_projection::ore_projection(std::unordered_map<std::string, std::string> curve_names)
    : curve_names_(std::move(curve_names)) {}

std::expected<std::string, std::string>
ore_projection::ore_name_of(const datum::market_reference& reference) const {
    if (const auto* index = reference.index())
        return datum::ore_index_codec::write(*index);

    const auto* datum = reference.datum();
    if (!datum->is_series())
        return datum::ore_key_codec::write(*datum);

    const auto named = curve_names_.find(reference.uri());
    if (named == curve_names_.end())
        return std::unexpected(std::format(
            "'{}': the market configuration gives this curve no ORE name", reference.uri()));
    return named->second;
}

std::string ore_projection::ore_name_or_stored(std::string_view stored) const {
    const auto reference = datum::market_reference::parse(stored);
    if (!reference)
        return std::string(stored);
    auto name = ore_name_of(*reference);
    return name ? std::move(*name) : std::string(stored);
}

}
