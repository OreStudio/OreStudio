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
#ifndef ORES_MARKETDATA_CORE_VENDOR_WMR_CUT_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_WMR_CUT_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <string_view>

namespace ores::marketdata::vendor {

/**
 * @brief The notation of the WM/Reuters benchmark publisher.
 *
 * The WM/Reuters benchmarks fix a currency pair at seven cuts a day, and the cut
 * is part of the fixing's identity: the same pair at the London and the New York
 * cut is two numbers taken at two times. The id carries the cut's own key and the
 * pair, such as =WMR1000NYC EURUSD=, and the notation reads and writes the fx
 * fixing item the two belong to, such as
 * =oresmd://fx/EUR?type=fixing&index=fx&source=WMR1000NYC&ccy=USD=.
 */
class ORES_MARKETDATA_CORE_EXPORT wmr_cut_notation final : public provider_notation {
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
