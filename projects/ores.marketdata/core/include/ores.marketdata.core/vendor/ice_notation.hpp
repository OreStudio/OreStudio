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
#ifndef ORES_MARKETDATA_CORE_VENDOR_ICE_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_ICE_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"

namespace ores::marketdata::vendor {

/**
 * @brief The notation of the provider ice.
 *
 * ICE Data Services is a simulated provider here, so this is the spelling we write for it and not
 * a claim about ICE's Consolidated Feed symbology. It writes one pillar of an interest rate curve
 * the way the provider's LIBOR curves are named: the currency, the index family and the tenor of
 * the index itself together, then the instrument, the tenor the pillar starts at and the tenor it
 * ends at, each its own dot separated word, with the final equals sign: "USDLIBOR3M.DEPO.SPOT.6M="
 * or "USDLIBOR3M.FRA.6M.12M=". The index family is ICE's own benchmark name in upper case, such as
 * LIBOR, and it reads back in the lower case the refdata catalogue spells it in.
 *
 * The index tenor is run into the family word because ICE names a LIBOR curve by its tenor. It
 * keeps two curves of one currency apart, which differ only in that tenor, as the USD one and six
 * month curves do. The family is the leading upper case letters and the tenor begins with a digit,
 * so the two read apart without a second separator. The tenors are the periods the provider quotes,
 * such as 1M, 3M and 6M.
 *
 * A curve pillar carries one price and ICE quotes it at the bid, so its field is BID. The notation
 * writes no currency pair: ICE's simulated feeds quote interest rate curves only.
 *
 * The reader is a Boost.Spirit.X3 grammar, and the notation follows the contract of
 * @c provider_notation: it reads what it writes and writes what it reads.
 */
class ORES_MARKETDATA_CORE_EXPORT ice_notation final : public provider_notation {
public:
    [[nodiscard]] std::string_view provider() const override;
    [[nodiscard]] std::string_view id_scheme() const override;
    [[nodiscard]] price_side primary_side() const override;
    [[nodiscard]] bool names_index_tenor() const override;
    [[nodiscard]] std::expected<quote_description, notation_error>
    read(const messaging::vendor_id& id) const override;
    [[nodiscard]] std::expected<messaging::vendor_id, notation_error>
    write(const quote_description& description) const override;
};

}

#endif
