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
#ifndef ORES_MARKETDATA_CORE_VENDOR_FIXING_SETTING_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_FIXING_SETTING_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <string>

namespace ores::marketdata::vendor {

/**
 * @brief The notation of a publisher that fixes an interest rate setting.
 *
 * A rate administrator names a setting by its currency, its index name and, for a
 * term index, its tenor: =USD LIBOR 3M=, =EUR EURIBOR 6M=, =CHF SARON=. The id
 * carries those three and the field names the rate, so the notation reads and
 * writes the fixing item the setting belongs to, such as
 * =oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M=.
 *
 * One implementation serves every such publisher, because the grammar is the
 * market's and not a vendor's: the administrator is the provider of the row, and
 * the catalogue decides which settings that provider serves.
 */
class ORES_MARKETDATA_CORE_EXPORT fixing_setting_notation final : public provider_notation {
public:
    explicit fixing_setting_notation(std::string provider);

    [[nodiscard]] std::string_view provider() const override;
    [[nodiscard]] std::string_view id_scheme() const override;
    [[nodiscard]] price_side primary_side() const override;
    [[nodiscard]] std::expected<quote_description, notation_error>
    read(const messaging::vendor_id& id) const override;
    [[nodiscard]] std::expected<messaging::vendor_id, notation_error>
    write(const quote_description& description) const override;

private:
    std::string provider_;
};

}

#endif
