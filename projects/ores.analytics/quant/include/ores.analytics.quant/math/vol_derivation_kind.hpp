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
#ifndef ORES_ANALYTICS_QUANT_MATH_VOL_DERIVATION_KIND_HPP
#define ORES_ANALYTICS_QUANT_MATH_VOL_DERIVATION_KIND_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/volatility_measurement.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <span>
#include <string_view>

namespace ores::analytics::quant::math {

/**
 * @file vol_derivation_kind.hpp
 * @brief What a class declares about its derivation, in one row.
 *
 * A derivation is one pattern with a different methodology behind each class. The measurement is
 * the engine's; what the class states is three things -- the transform, the coordinates and the
 * smile model -- plus the identity a stored value carries so it can say how it was made.
 *
 * This file is that declaration and nothing else. It holds no arithmetic, no store and no feed, so
 * the emitter, the batch runner and the page can read a kind without depending on the class's
 * module, and adding a class is a row here and a module beside it.
 */

/// The identity a series stores, and the four things the class states.
struct vol_derivation_kind final {
    /// The code a series states as its derivation kind, such as =FX_VOL_DERIVATION=.
    std::string_view code;
    /// The version of the computation. It rises when the arithmetic changes, so a stored value
    /// names the method that wrote it and not merely the method that exists.
    int version;
    /// The identifier a stored value records as its derivation configuration, in canonical text.
    /// It is fixed for the life of the kind: changing it would make an old value unreadable.
    std::string_view method_id;
    /// The refdata asset class the kind serves.
    std::string_view asset_class;
    /// How the class's level series becomes returns.
    volatility_transform transform;
    /// The datum fields that name one quote, in the order the class reads them. They are the
    /// coordinates the emitter writes and the page draws, so a page that reads them needs no
    /// branch on the class.
    std::span<const std::string_view> coordinates;
    /// The smile model the class fits across strikes, or the delta labels it reads instead.
    std::string_view smile_model;
};

/// The FX kind: a pair's spot log returns, to a smile in delta.
ORES_ANALYTICS_QUANT_EXPORT const vol_derivation_kind& fx_vol_derivation();

/// The interest rate kind: a term's rate differences, to a smile in strike.
ORES_ANALYTICS_QUANT_EXPORT const vol_derivation_kind& ir_vol_derivation();

/**
 * @brief Every kind the platform derives, in one table.
 *
 * A kind that is not here is not derived, which is how a caller refuses a class it has no module
 * for: it asks for the kind and names the code it was given.
 */
ORES_ANALYTICS_QUANT_EXPORT std::span<const vol_derivation_kind> vol_derivation_kinds();

/// The kind a code names, or nothing.
ORES_ANALYTICS_QUANT_EXPORT std::optional<vol_derivation_kind>
vol_derivation_kind_named(std::string_view code);

/// The identifier a kind's method records, parsed from the canonical text it declares.
ORES_ANALYTICS_QUANT_EXPORT boost::uuids::uuid method_id_of(const vol_derivation_kind& kind);

}

#endif
