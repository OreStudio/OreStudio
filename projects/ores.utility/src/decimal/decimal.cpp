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
#include "ores.utility/decimal/decimal.hpp"
#include <cctype>
#include <charconv>
#include <cmath>
#include <limits>
#include <optional>
#include <system_error>
#include <utility>

namespace ores::utility::decimal {

namespace {

using value_type = boost::multiprecision::cpp_dec_float_50;

bool is_digit(char c) {
    return std::isdigit(static_cast<unsigned char>(c)) != 0;
}

/**
 * @brief Splits decimal text into its sign, digits and exponent.
 *
 * Accepts the spellings the type states it accepts and nothing else:
 * an optional sign, an integer part or a fractional part (at least
 * one of the two, and a fractional part only with its point), and an
 * optional decimal exponent. nan, inf, whitespace and a repeated
 * point fail here rather than reaching the number parser, which would
 * accept the non-finite spellings.
 */
struct parts {
    bool negative = false;
    std::string digits;
    long point = 0;
};

std::optional<parts> split(std::string_view str) {
    if (str.empty())
        return std::nullopt;

    std::size_t i = 0;
    parts p;
    if (str[i] == '+' || str[i] == '-') {
        p.negative = (str[i] == '-');
        ++i;
    }

    const std::size_t int_begin = i;
    while (i < str.size() && is_digit(str[i]))
        ++i;
    const std::size_t int_end = i;

    std::size_t frac_begin = i;
    std::size_t frac_end = i;
    if (i < str.size() && str[i] == '.') {
        ++i;
        frac_begin = i;
        while (i < str.size() && is_digit(str[i]))
            ++i;
        frac_end = i;
    }

    if (int_end == int_begin && frac_end == frac_begin)
        return std::nullopt;

    long exponent = 0;
    if (i < str.size() && (str[i] == 'e' || str[i] == 'E')) {
        ++i;
        bool negative_exponent = false;
        if (i < str.size() && (str[i] == '+' || str[i] == '-')) {
            negative_exponent = (str[i] == '-');
            ++i;
        }
        const std::size_t exp_begin = i;
        while (i < str.size() && is_digit(str[i]))
            ++i;
        if (i == exp_begin)
            return std::nullopt;
        for (std::size_t k = exp_begin; k < i; ++k)
            exponent = exponent * 10 + (str[k] - '0');
        if (negative_exponent)
            exponent = -exponent;
    }

    if (i != str.size())
        return std::nullopt;

    p.digits = std::string(str.substr(int_begin, int_end - int_begin)) +
               std::string(str.substr(frac_begin, frac_end - frac_begin));
    p.point = static_cast<long>(int_end - int_begin) + exponent;
    return p;
}

std::string::size_type significant_digit_count(const std::string& digits) {
    const auto first = digits.find_first_not_of('0');
    if (first == std::string::npos)
        return 0;
    const auto last = digits.find_last_not_of('0');
    return last - first + 1;
}

/**
 * @brief Renders a decimal in plain notation.
 *
 * The library's own rendering is the shortest text that round trips,
 * which is what an exact decimal wants, but it spells a small or large
 * magnitude with a scientific exponent. A money amount is read and
 * written as plain text, so the exponent becomes a point shift and the
 * insignificant zeros around the digits are dropped.
 */
std::string to_plain_string(const value_type& v) {
    const std::string s = v.str(0, std::ios_base::fmtflags(0));
    const auto p = split(s);
    if (!p)
        return s;

    std::string int_part;
    std::string frac_part;
    if (p->point <= 0) {
        int_part = "0";
        frac_part = std::string(static_cast<std::size_t>(-p->point), '0') + p->digits;
    } else if (static_cast<std::size_t>(p->point) >= p->digits.size()) {
        int_part =
            p->digits + std::string(static_cast<std::size_t>(p->point) - p->digits.size(), '0');
    } else {
        const auto point = static_cast<std::size_t>(p->point);
        int_part = p->digits.substr(0, point);
        frac_part = p->digits.substr(point);
    }

    const auto first = int_part.find_first_not_of('0');
    int_part = (first == std::string::npos) ? "0" : int_part.substr(first);
    const auto last = frac_part.find_last_not_of('0');
    frac_part = (last == std::string::npos) ? std::string{} : frac_part.substr(0, last + 1);

    std::string r;
    if (p->negative && !(int_part == "0" && frac_part.empty()))
        r.push_back('-');
    r += int_part;
    if (!frac_part.empty()) {
        r.push_back('.');
        r += frac_part;
    }
    return r;
}

}

decimal::decimal(value_type v)
    : value_(std::move(v)) {}

std::expected<decimal, std::string> decimal::from_string(std::string_view str) {
    const auto p = split(str);
    if (!p)
        return std::unexpected("A decimal is text such as \"-12.5\" or \"1e-10\", got: \"" +
                               std::string(str) + "\"");

    constexpr std::size_t held = std::numeric_limits<value_type>::digits10;
    if (significant_digit_count(p->digits) > held) {
        return std::unexpected("A decimal holds " + std::to_string(held) +
                               " significant digits, and this text states more: \"" +
                               std::string(str) + "\" would be rounded silently");
    }

    try {
        return decimal(value_type(std::string(str)));
    } catch (const std::exception& e) {
        return std::unexpected("Unable to read the decimal \"" + std::string(str) +
                               "\": " + e.what());
    }
}

std::expected<decimal, std::string> decimal::from_double(double v) {
    if (!std::isfinite(v))
        return std::unexpected("A decimal is finite, and this value is not");

    char buffer[64];
    const auto result = std::to_chars(buffer, buffer + sizeof(buffer), v);
    if (result.ec != std::errc{})
        return std::unexpected("Unable to render the value as decimal text");
    return from_string(std::string_view(buffer, static_cast<std::size_t>(result.ptr - buffer)));
}

std::string decimal::to_string() const {
    return to_plain_string(value_);
}

double decimal::to_double() const {
    return static_cast<double>(value_);
}

int decimal::scale() const {
    const auto text = to_string();
    const auto point = text.find('.');
    if (point == std::string::npos)
        return 0;
    return static_cast<int>(text.size() - point - 1);
}

bool decimal::is_zero() const {
    return value_ == 0;
}

std::ostream& operator<<(std::ostream& os, const decimal& v) {
    return os << v.to_string();
}

}
