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
#ifndef ORES_MARKETDATA_CORE_DATUM_ORE_KEY_CODEC_HPP
#define ORES_MARKETDATA_CORE_DATUM_ORE_KEY_CODEC_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/export.hpp"
#include <expected>
#include <string>
#include <string_view>

namespace ores::marketdata::datum {

/**
 * @brief Reads ORE market data keys into market datums and writes them back.
 *
 * The reader follows ORE's own parseMarketDatum, form by form: each key form
 * ORE accepts is accepted here, each token goes to the field ORE puts it in, and
 * a field is chosen by what a token is rather than by where it sits. The writer
 * is the inverse: the instrument and quote tokens, then the datum's fields in
 * schema order, leaving out the ones that hold none.
 *
 * write(read(k)) is k, with one exception: a key that spells its type or quote
 * type with an alias ORE also reads (FX_SPOT for FX, FX_FWD for FXFWD,
 * RATE_GVOL for RATE_LNVOL) is written in the canonical spelling.
 *
 * Two kinds of key ORE accepts are refused, because the datum could not write
 * them back: a key with tokens ORE ignores (a trailing token after a cap/floor
 * shift, a sixth token on a base correlation tranche), and a numeric slot that
 * holds text ORE's std::stod would read only in part.
 */
class ORES_MARKETDATA_CORE_EXPORT ore_key_codec final {
public:
    /// The datum @p key names, or why ORE's grammar does not admit it.
    [[nodiscard]] static std::expected<market_datum, std::string> read(std::string_view key);

    /// The key for @p datum; @p datum must not be a series.
    [[nodiscard]] static std::string write(const market_datum& datum);

    /// The first token of a key of type @p t: FX for fx_spot, EQUITY for
    /// equity_spot, ZC_INFLATIONSWAP for zc_inflation_swap.
    [[nodiscard]] static std::string_view token_of(instrument_type t);
};

}

#endif
