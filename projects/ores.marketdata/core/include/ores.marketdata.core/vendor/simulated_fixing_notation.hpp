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
#ifndef ORES_MARKETDATA_CORE_VENDOR_SIMULATED_FIXING_NOTATION_HPP
#define ORES_MARKETDATA_CORE_VENDOR_SIMULATED_FIXING_NOTATION_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <string_view>

namespace ores::marketdata::vendor {

/// The provider whose source publishes the fixings the simulator states.
inline constexpr std::string_view simulated_fixings_provider = "simulated-fixings";

/**
 * @brief The notation of the simulated fixing publisher.
 *
 * The publisher fixes indices no real publisher serves the run, so it has no vendor's own
 * notation. It names a fixing by its oresmd address, which carries the index's market name,
 * such as oresmd://ir/EUR?type=fixing&index=ibor&name=ESTR, with the field "value". It writes
 * only fixings, and reads an id only when its address names a fixing and is written in the
 * codec's own spelling.
 */
class ORES_MARKETDATA_CORE_EXPORT simulated_fixing_notation final : public provider_notation {
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
