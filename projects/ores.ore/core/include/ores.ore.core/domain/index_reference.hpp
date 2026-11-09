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
#ifndef ORES_ORE_CORE_DOMAIN_INDEX_REFERENCE_HPP
#define ORES_ORE_CORE_DOMAIN_INDEX_REFERENCE_HPP

#include "ores.marketdata.api/projection/ore_projection.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::ore::domain {

/// The address a trade stores for the ORE index name a document states.
inline std::string address_of(std::string_view ore_name) {
    return ores::marketdata::projection::ore_projection::address_of_index_or_stored(ore_name);
}

/// The same, for a name the document may leave out.
inline std::optional<std::string> address_of_optional(const std::optional<std::string>& ore_name) {
    if (!ore_name)
        return std::nullopt;
    return address_of(*ore_name);
}

/// An index converts by a fixed rule, so its name needs no market configuration.
inline std::string ore_name_of(std::string_view stored) {
    static const ores::marketdata::projection::ore_projection projection;
    return projection.ore_name_or_stored(stored);
}

}

#endif
