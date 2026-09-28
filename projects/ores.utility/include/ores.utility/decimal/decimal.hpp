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
#ifndef ORES_UTILITY_DECIMAL_DECIMAL_HPP
#define ORES_UTILITY_DECIMAL_DECIMAL_HPP

#include "ores.utility/export.hpp"
#include <boost/multiprecision/cpp_dec_float.hpp>
#include <compare>
#include <expected>
#include <ostream>
#include <string>
#include <string_view>

namespace ores::utility::decimal {

/**
 * @brief An exact decimal number, for a monetary amount.
 *
 * A money amount is a decimal and never a double: the database already
 * stores these columns as numeric, and binary floating point cannot
 * hold a decimal fraction such as 0.1 exactly. This type is the decimal
 * the domain carries. Rates, volatilities and correlations are not
 * money and stay double.
 *
 * @details The primitive is boost::multiprecision::cpp_dec_float_50, a
 * decimal floating point whose mantissa is held in base ten, so every
 * decimal fraction it accepts is stored exactly and no value ever
 * passes through a binary float. Fifty significant decimal digits is
 * wider than the widest numeric column in the schema (28), so a
 * column's own precision is the only limit a stored amount meets.
 *
 * The type owns its text. to_string() renders one canonical plain
 * decimal spelling -- never a scientific exponent, never a trailing
 * zero -- and from_string() accepts one back. The two are inverses, so
 * a value that leaves as text returns as the same value. The SQL
 * numeric(p, s) column bounds the value it stores; this type does
 * not know a column's precision and does not invent one.
 *
 * A decimal is never absent. A nullable column declares
 * std::optional<decimal>, and the optional states the absence, so a
 * default-constructed decimal is exactly zero and not a sentinel.
 */
class ORES_UTILITY_EXPORT decimal final {
public:
    /**
     * @brief Constructs the decimal zero.
     */
    decimal() = default;

    /**
     * @brief Parses an exact decimal from its text.
     *
     * @details Accepts an optional sign, an integer part, a fractional
     * part, and an optional decimal exponent (=1e-10=). The text must
     * be a decimal number: whitespace, an empty string, nan, inf
     * and any other spelling are refused, and so is a text carrying
     * more significant digits than the representation holds, because
     * accepting it would round the value silently.
     *
     * @param str The decimal text.
     * @return The decimal, or the reason the text is not one.
     */
    static std::expected<decimal, std::string> from_string(std::string_view str);

    /**
     * @brief Builds a decimal from a value a legacy source already holds
     * as a double.
     *
     * @details Exists for the one boundary that hands the domain a
     * binary float it did not choose -- the ORE XML import, whose number
     * types are floats. The double's shortest round-tripping decimal
     * spelling is what is stored, and the value is a decimal from there
     * on. It is not the way a decimal is normally built; a caller that
     * holds text parses the text with from_string().
     *
     * @param v The value to convert. Must be finite.
     * @return The decimal, or the reason the value cannot be one.
     */
    static std::expected<decimal, std::string> from_double(double v);

    /**
     * @brief Renders the canonical plain decimal text.
     *
     * @details Plain notation, so never a scientific exponent; no
     * leading zeros and no trailing fractional zeros; the sign kept and
     * the zero unsigned. The rendering is what the wire carries and
     * what the database column stores, and from_string() reads it back
     * unchanged.
     */
    [[nodiscard]] std::string to_string() const;

    /**
     * @brief The value as a double, for a boundary that still holds a
     * binary float.
     *
     * @details The ORE XML number type is a float, so the export path
     * that rebuilds an ORE document from a stored amount has to hand it
     * a double. This is a named boundary conversion, the inverse of
     * from_double(), and it loses whatever a double cannot hold; nothing
     * inside the domain converts through it.
     */
    [[nodiscard]] double to_double() const;

    /**
     * @brief The number of fractional digits the canonical text carries.
     *
     * @details Zero for an integral value. This is the scale the type
     * owns: it follows the value's own digits, not a column's declared
     * scale, so a value parsed from "0.1000" reports one.
     */
    [[nodiscard]] int scale() const;

    /**
     * @brief Whether the value is exactly zero.
     */
    [[nodiscard]] bool is_zero() const;

    friend bool operator==(const decimal& lhs, const decimal& rhs) noexcept {
        return lhs.value_ == rhs.value_;
    }

    friend std::strong_ordering operator<=>(const decimal& lhs, const decimal& rhs) noexcept {
        if (lhs.value_ < rhs.value_)
            return std::strong_ordering::less;
        if (rhs.value_ < lhs.value_)
            return std::strong_ordering::greater;
        return std::strong_ordering::equal;
    }

private:
    using value_type = boost::multiprecision::cpp_dec_float_50;

    explicit decimal(value_type v);

    value_type value_{0};
};

/**
 * @brief Writes the canonical decimal text.
 */
ORES_UTILITY_EXPORT std::ostream& operator<<(std::ostream& os, const decimal& v);

}

#endif
