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
#ifndef ORES_REFDATA_CORE_MAPPING_FX_OPTION_CONVENTION_MAPPING_HPP
#define ORES_REFDATA_CORE_MAPPING_FX_OPTION_CONVENTION_MAPPING_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/math/fx_smile_quotes.hpp"
#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.refdata.core/export.hpp"
#include <expected>
#include <string>
#include <string_view>

namespace ores::refdata::mapping {

/**
 * @brief A tenor as a convention or a quote writes it, such as 1W, 6M or 2Y, as a period.
 *
 * Refuses text that is not a count followed by D, W, M or Y.
 */
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::expected<analytics::quant::domain::date_period, std::string>
parse_tenor_text(std::string_view text);

/**
 * @brief The quoting convention an FX option convention states for an option of @p expiry.
 *
 * An expiry at or past the switch tenor takes the long-term ATM and delta types, where the
 * convention states them, compared as QuantLib compares periods. A convention with no switch
 * tenor, or a switch of zero length, keeps its short-term types at every expiry, as ORE does. ORE's spellings are read:
 * AtmDeltaNeutral and AtmFwd; Spot, Fwd, PaSpot and PaFwd; a risk reversal in favour of Call or
 * Put, Call when unstated; a Broker or Smile butterfly, Smile when unstated. Refuses a type the
 * quant library does not model, a switch tenor that is not a tenor, and an expiry the switch
 * tenor cannot be ordered against, naming each.
 */
[[nodiscard]] ORES_REFDATA_CORE_EXPORT std::expected<analytics::quant::math::fx_quote_convention, std::string>
to_quote_convention(const domain::fx_option_convention& convention,
                    analytics::quant::domain::date_period expiry);

}

#endif
