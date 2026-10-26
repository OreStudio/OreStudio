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
#ifndef ORES_MARKETDATA_CORE_VENDOR_DEMO_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_DEMO_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"

namespace ores::marketdata::vendor {

/**
 * @brief The notation of the provider demo.
 *
 * The fictional provider Demo writes the spot quote of a pair as the pair with a slash, such as
 * "EUR/USD", and the field "value". It has one price per pair, which the notation reads as the last
 * price. It exists for exercises and demonstrations.
 *
 * Demo writes one pillar of an interest rate curve as the currency, the index family, the
 * instrument, the tenor the pillar starts at and the tenor it ends at, each its own slash separated
 * word, such as "USD/sofr/DEPO/SPOT/3M" or "USD/sofr/FRA/3M/6M", and the field "value". The index
 * family is the refdata catalogue's own lower case, as the Demo configs spell it, so the id keeps
 * the case the catalogue holds.
 *
 * The reader is a Boost.Spirit.X3 grammar, and the notation follows the contract of
 * @c provider_notation: it reads what it writes and writes what it reads.
 */
class ORES_MARKETDATA_CORE_EXPORT demo_notation final : public provider_notation {
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
