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
#ifndef ORES_UTILITY_DIGEST_CANONICAL_FORM_HPP
#define ORES_UTILITY_DIGEST_CANONICAL_FORM_HPP

#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <format>
#include <iterator>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::utility::digest {

template <typename>
inline constexpr bool always_false = false;

/**
 * @brief Frames a value by its length, so a value never runs into the next.
 *
 * Two different values cannot frame to the same text.
 */
inline std::string frame(std::string_view payload) {
    std::string out;
    out.append(std::to_string(payload.size()));
    out.push_back(':');
    out.append(payload);
    return out;
}

template <typename E>
std::string canonical_enum(const E& v) {
    if constexpr (requires { to_string(v); })
        return frame(to_string(v));
    else
        return frame(std::to_string(static_cast<std::underlying_type_t<E>>(v)));
}

/**
 * @brief Renders one value canonically.
 *
 * The same value always gives the same bytes: a decimal renders its own text
 * and never passes through a double. A struct renders through its
 * =canonical_form=, which codegen writes from the model's :economic: property.
 */
template <typename T>
std::string canonical_value(const T& v) {
    using V = std::remove_cvref_t<T>;

    if constexpr (std::is_same_v<V, ores::utility::decimal::decimal>) {
        return frame(v.to_string());
    } else if constexpr (std::is_same_v<V, bool>) {
        return frame(v ? "true" : "false");
    } else if constexpr (std::is_integral_v<V>) {
        return frame(std::to_string(v));
    } else if constexpr (std::is_floating_point_v<V>) {
        return frame(std::format("{:.17g}", v));
    } else if constexpr (std::is_enum_v<V>) {
        return canonical_enum(v);
    } else if constexpr (std::is_same_v<V, std::string> || std::is_same_v<V, std::string_view>) {
        return frame(v);
    } else if constexpr (std::is_same_v<V, std::chrono::year_month_day>) {
        return frame(std::format("{:04d}-{:02d}-{:02d}",
                                 static_cast<int>(v.year()),
                                 static_cast<unsigned>(v.month()),
                                 static_cast<unsigned>(v.day())));
    } else if constexpr (std::is_same_v<V, std::chrono::system_clock::time_point>) {
        // A time renders by its tick count, not by a calendar format, so the
        // text depends on the value alone and not on a locale or a timezone.
        return frame(std::to_string(static_cast<long long>(v.time_since_epoch().count())));
    } else if constexpr (requires {
                             v.has_value();
                             *v;
                         }) {
        if (!v.has_value())
            return std::string("none");
        return std::string("some") + canonical_value(*v);
    } else if constexpr (requires {
                             v.begin();
                             v.end();
                             typename V::value_type;
                         }) {
        std::string out = "seq";
        out.append(std::to_string(std::distance(v.begin(), v.end())));
        out.push_back('{');
        std::size_t index = 0;
        for (const auto& element : v) {
            out.append(std::to_string(index));
            out.push_back('=');
            out.append(canonical_value(element));
            out.push_back(';');
            ++index;
        }
        out.push_back('}');
        return out;
    } else if constexpr (requires { canonical_form(v); }) {
        return canonical_form(v);
    } else {
        static_assert(always_false<V>,
                      "canonical_value cannot render this type. A struct needs a "
                      "canonical_form: add ':economic: true' or ':economic: false' to "
                      "every column of its model.");
    }
}

/**
 * @brief Appends one member to a canonical form.
 *
 * The member carries its own name, as a plain identifier, then its canonical
 * value, already framed, so the name and the value never merge.
 */
template <typename T>
void append_member(std::string& out, std::string_view name, const T& value) {
    out.append(name);
    out.push_back('=');
    out.append(canonical_value(value));
    out.push_back(';');
}

}

#endif
