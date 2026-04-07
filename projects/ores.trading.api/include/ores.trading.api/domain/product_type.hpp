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
#ifndef ORES_TRADING_DOMAIN_PRODUCT_TYPE_HPP
#define ORES_TRADING_DOMAIN_PRODUCT_TYPE_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <stdexcept>
#include <rfl.hpp>

namespace ores::trading::domain {

/**
 * @brief Structural routing discriminator for trades and instruments.
 *
 * Mirrors the @c product_type_t Postgres enum and partitions all trade types
 * into one of eight families. The family selects which extension table the
 * instrument record lives in and which @c IInstrumentForm the Qt UI shows.
 *
 * String form (used on the wire and in SQL) is the lowercase enumerator name.
 */
enum class product_type : std::uint8_t {
    swap,
    fx,
    bond,
    credit,
    equity,
    commodity,
    composite,
    scripted
};

/**
 * @brief Convert a product_type to its lowercase string representation.
 *
 * The result matches the SQL @c product_type_t enum values and the JSON
 * wire format. Throws @c std::invalid_argument on an unknown value.
 */
[[nodiscard]] inline std::string_view to_string(product_type pt) {
    switch (pt) {
    case product_type::swap:      return "swap";
    case product_type::fx:        return "fx";
    case product_type::bond:      return "bond";
    case product_type::credit:    return "credit";
    case product_type::equity:    return "equity";
    case product_type::commodity: return "commodity";
    case product_type::composite: return "composite";
    case product_type::scripted:  return "scripted";
    }
    throw std::invalid_argument("Unknown product_type");
}

/**
 * @brief Parse a product_type from its lowercase string representation.
 *
 * Returns @c std::nullopt for unrecognised values (callers can decide whether
 * that is an error or simply "no family selected").
 */
[[nodiscard]] inline std::optional<product_type>
product_type_from_string(std::string_view sv) {
    if (sv == "swap")      return product_type::swap;
    if (sv == "fx")        return product_type::fx;
    if (sv == "bond")      return product_type::bond;
    if (sv == "credit")    return product_type::credit;
    if (sv == "equity")    return product_type::equity;
    if (sv == "commodity") return product_type::commodity;
    if (sv == "composite") return product_type::composite;
    if (sv == "scripted")  return product_type::scripted;
    return std::nullopt;
}

}

namespace rfl {

/**
 * @brief rfl reflector for ores::trading::domain::product_type.
 *
 * Serialises to/from a lowercase string in JSON and any other rfl-driven
 * wire format. Wire shape stays identical to the previous @c std::string
 * field, so no protocol changes are required when consumers migrate.
 */
template<>
struct Reflector<ores::trading::domain::product_type> {
    using ReflType = std::string;

    static ores::trading::domain::product_type to(const ReflType& s) {
        auto parsed = ores::trading::domain::product_type_from_string(s);
        if (!parsed) {
            throw std::invalid_argument("Invalid product_type: '" + s + "'");
        }
        return *parsed;
    }

    static ReflType from(const ores::trading::domain::product_type& v) {
        return std::string(ores::trading::domain::to_string(v));
    }
};

}

#endif
