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
#ifndef ORES_MARKETDATA_CORE_VENDOR_BLOOMBERG_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_BLOOMBERG_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"

namespace ores::marketdata::vendor {

/**
 * @brief The notation of the provider bloomberg.
 *
 * Bloomberg writes the spot quote of a pair as a ticker and a field. The ticker is the pair as six
 * upper case letters, a space and the yellow key Curncy, such as "EURUSD Curncy". The field names
 * the side: PX_LAST, PX_BID, PX_ASK or PX_MID. The ticker admits any order of the two currencies,
 * and a pricing source between the pair and the key ("EURUSD BGN Curncy") is not described, so the
 * notation refuses it.
 *
 * Bloomberg writes one pillar of an interest rate curve as the currency, the index family, the
 * instrument, the tenor the pillar starts at and the tenor it ends at, each a word before the
 * yellow key, such as "USD SOFR DEPO SPOT 3M Curncy" or "USD SOFR FRA 3M 6M Curncy". A curve
 * pillar carries one price, so its field is PX_LAST. The index family is written in upper case
 * and read back in the lower case the refdata catalogue spells it in.
 *
 * The reader is a Boost.Spirit.X3 grammar, and the notation follows the contract of
 * @c provider_notation: it reads what it writes and writes what it reads.
 */
class ORES_MARKETDATA_CORE_EXPORT bloomberg_notation final : public provider_notation {
public:
    [[nodiscard]] std::string_view provider() const override;
    [[nodiscard]] std::string_view id_scheme() const override;
    [[nodiscard]] price_side primary_side() const override;
    [[nodiscard]] std::expected<quote_description, notation_error>
    read(const messaging::vendor_id& id) const override;
    [[nodiscard]] std::expected<messaging::vendor_id, notation_error>
    write(const quote_description& description) const override;
};

}

#endif
