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
#include "ores.analytics.quant/math/vol_derivation_kind.hpp"
#include <boost/uuid/string_generator.hpp>
#include <array>

namespace ores::analytics::quant::math {

namespace {

/// The fields an FX option quote is named by, in the order the key writes them.
constexpr std::array<std::string_view, 4> fx_coordinates{
    "unit_ccy", "ccy", "expiry", "strike_label"};

/// The fields an interest rate swaption quote is named by, in the order the key writes them.
constexpr std::array<std::string_view, 4> ir_coordinates{"ccy", "expiry", "term", "strike_level"};

constexpr vol_derivation_kind fx_kind{.code = "FX_VOL_DERIVATION",
                                      .version = 1,
                                      .method_id = "fefdf018-d882-5c4e-bd27-2aeedfcad9c9",
                                      .asset_class = "fx",
                                      .transform = volatility_transform::log_return,
                                      .coordinates = fx_coordinates,
                                      .smile_model = "gram_charlier_delta"};

constexpr vol_derivation_kind ir_kind{.code = "IR_VOL_DERIVATION",
                                      .version = 1,
                                      .method_id = "2869a43f-2202-5b12-9e30-cfd8d5df3500",
                                      .asset_class = "interest_rates",
                                      .transform = volatility_transform::difference,
                                      .coordinates = ir_coordinates,
                                      .smile_model = "gram_charlier_normal_skew"};

constexpr std::array<vol_derivation_kind, 2> kinds{fx_kind, ir_kind};

}

const vol_derivation_kind& fx_vol_derivation() {
    return fx_kind;
}

const vol_derivation_kind& ir_vol_derivation() {
    return ir_kind;
}

std::span<const vol_derivation_kind> vol_derivation_kinds() {
    return kinds;
}

std::optional<vol_derivation_kind> vol_derivation_kind_named(std::string_view code) {
    for (const auto& kind : kinds)
        if (kind.code == code)
            return kind;
    return std::nullopt;
}

boost::uuids::uuid method_id_of(const vol_derivation_kind& kind) {
    return boost::uuids::string_generator{}(kind.method_id);
}

}
