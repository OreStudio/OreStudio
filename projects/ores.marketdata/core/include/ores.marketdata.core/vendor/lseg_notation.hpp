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
#ifndef ORES_MARKETDATA_CORE_VENDOR_LSEG_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_LSEG_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"

namespace ores::marketdata::vendor {

/**
 * @brief The notation of the provider lseg.
 *
 * LSEG writes the spot quote of a pair as a RIC and a field. A pair that holds USD is written with
 * the other currency and an equals sign ("EUR=" for EUR/USD, "JPY=" for USD/JPY), and any other
 * pair with both currencies and an equals sign ("EURGBP="). The RIC does not say which currency is
 * the base of a USD pair, so the notation holds the market convention: EUR, GBP, AUD and NZD are
 * quoted as the base against USD, and every other currency is quoted as the quote of USD. The field
 * names the side: BID, ASK or TRDPRC_1 for the last trade. A mid price has no field here. The rows
 * are the documented examples, and a person with a login confirms them.
 *
 * The reader is a Boost.Spirit.X3 grammar, and the notation follows the contract of
 * @c provider_notation: it reads what it writes and writes what it reads.
 */
class ORES_MARKETDATA_CORE_EXPORT lseg_notation final : public provider_notation {
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
