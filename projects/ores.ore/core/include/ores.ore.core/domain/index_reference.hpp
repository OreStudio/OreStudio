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

/// The index family that names an underlying of an ORE underlying type.
struct underlying_family final {
    std::string_view type;
    std::string_view prefix;
};

inline constexpr underlying_family underlying_families[] = {
    {"Equity", "EQ-"}, {"Commodity", "COMM-"}, {"FX", "FX-"}, {"Bond", "BOND-"}};

/**
 * @brief The address a trade stores for an underlying of the type.
 *
 * ORE names an underlying by its bare subject, so the address takes its family
 * from the type. A name that a family cannot hold and return stays as written.
 */
inline std::string underlying_address_of(std::string_view type, std::string_view ore_name) {
    for (const auto& family : underlying_families) {
        if (family.type != type)
            continue;
        if (ore_name.empty() || ore_name.starts_with(family.prefix))
            break;
        auto address = address_of(std::string(family.prefix).append(ore_name));
        if (address.starts_with("oresmd:"))
            return address;
        break;
    }
    return std::string(ore_name);
}

inline std::string equity_address_of(std::string_view ore_name) {
    return underlying_address_of("Equity", ore_name);
}

inline std::string commodity_address_of(std::string_view ore_name) {
    return underlying_address_of("Commodity", ore_name);
}

inline std::string fx_address_of(std::string_view ore_name) {
    return underlying_address_of("FX", ore_name);
}

/// The bare name ORE states for an underlying a trade stores.
inline std::string underlying_name_of(std::string_view stored) {
    if (!stored.starts_with("oresmd:"))
        return std::string(stored);
    auto name = ore_name_of(stored);
    for (const auto& family : underlying_families)
        if (name.starts_with(family.prefix))
            return name.substr(family.prefix.size());
    return name;
}

/**
 * @brief A stored pair of equities.
 *
 * Addresses hold slashes, so a pair of them joins on a space. A pair is both
 * addresses or both names, so the first member decides how to split it.
 */
inline std::string equity_pair_of(std::string_view first, std::string_view second) {
    auto first_address = equity_address_of(first);
    auto second_address = equity_address_of(second);
    if (first_address.starts_with("oresmd:") && second_address.starts_with("oresmd:"))
        return first_address + " " + second_address;
    return std::string(first).append("/").append(second);
}

/// The separator of a stored pair, which a trade held as names joins on a slash.
inline char equity_pair_separator(std::string_view stored) {
    return stored.starts_with("oresmd:") ? ' ' : '/';
}

}

#endif
