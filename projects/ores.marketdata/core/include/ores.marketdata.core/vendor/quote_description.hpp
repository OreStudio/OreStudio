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
#ifndef ORES_MARKETDATA_CORE_VENDOR_QUOTE_DESCRIPTION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_QUOTE_DESCRIPTION_HPP

#include <string>
#include <variant>

namespace ores::marketdata::vendor {

/**
 * @brief Which side of the market a price is.
 *
 * Every provider names the side in the field of a vendor id: PX_LAST and PX_BID at Bloomberg, BID
 * and ASK at LSEG. The side is the neutral meaning of that field.
 */
enum class price_side { last, bid, ask, mid };

/**
 * @brief The spot quote of a currency pair, as a provider's id describes it.
 *
 * The pair is in the order the market quotes it, such as EUR/USD or USD/JPY. A notation that
 * cannot say which currency is the base refuses a pair that is not in market order.
 */
struct fx_spot_quote final {
    std::string base;
    std::string quote;
    price_side side = price_side::last;

    bool operator==(const fx_spot_quote&) const = default;
};

/**
 * @brief What a vendor id says about the quote it names, in no provider's words.
 *
 * Only the FX spot quote is described today. A rate pillar or another instrument is a further
 * alternative, and each notation then reads and writes it or refuses it.
 */
using quote_description = std::variant<fx_spot_quote>;

}

#endif
