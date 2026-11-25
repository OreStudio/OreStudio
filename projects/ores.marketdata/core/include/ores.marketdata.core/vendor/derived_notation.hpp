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
#ifndef ORES_MARKETDATA_CORE_VENDOR_DERIVED_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_DERIVED_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <string_view>

namespace ores::marketdata::vendor {

/// The provider whose source publishes the values the platform computes from
/// data it already holds.
inline constexpr std::string_view derived_provider = "derived";

/**
 * @brief The notation of the provider that states what we compute.
 *
 * It computes rather than observes, so it has no vendor's own notation. It names
 * an item by its oresmd address, which carries the market name, such as
 * oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M, with the field
 * "value". The scheme and the field are the fixing publisher's, because the
 * values are the same shape; only the provider differs, and the provider is what
 * decides whose namespace the id belongs to and whose subscription entitles it.
 */
class ORES_MARKETDATA_CORE_EXPORT derived_notation final : public provider_notation {
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
