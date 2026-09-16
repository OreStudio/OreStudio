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
#ifndef ORES_SHELL_APP_COMMAND_TOKEN_HPP
#define ORES_SHELL_APP_COMMAND_TOKEN_HPP

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::shell::app {

/**
 * @brief Token that leaves an optional column unset.
 *
 * The cli library splits a command line on whitespace, so an absent
 * argument cannot be typed as an empty token. A lone "-" stands in for
 * one, and every optional column accepts it.
 */
inline constexpr std::string_view absent_token = "-";

namespace detail {

template <typename T>
struct is_optional : std::false_type {};

template <typename T>
struct is_optional<std::optional<T>> : std::true_type {};

template <typename T>
inline constexpr bool is_optional_v = is_optional<T>::value;

template <typename T>
inline constexpr bool has_token_conversion_v = std::is_same_v<T, int> ||
    std::is_same_v<T, std::int64_t> || std::is_same_v<T, double> ||
    std::is_same_v<T, boost::uuids::uuid>;

template <typename T>
inline constexpr bool always_false_v = false;

}

/**
 * @brief Convert one command token into a value of a column's type.
 *
 * A std::optional column accepts the absent token to leave the value
 * unset. A std::string column takes the token verbatim, so a token that
 * looks like a number stays a string. Every other supported type goes
 * through boost::lexical_cast, which throws boost::bad_lexical_cast on
 * a malformed token.
 *
 * A column type with no conversion here fails to compile on purpose.
 * The generated command unit names the type, so the missing case is
 * visible at the point the entity is generated rather than at run time.
 */
template <typename T>
T from_token(const std::string& token) {
    if constexpr (detail::is_optional_v<T>) {
        if (std::string_view(token) == absent_token)
            return std::nullopt;
        return from_token<typename T::value_type>(token);
    } else if constexpr (std::is_same_v<T, std::string>) {
        return token;
    } else if constexpr (std::is_same_v<T, bool>) {
        return token == "true" || token == "1";
    } else if constexpr (detail::has_token_conversion_v<T>) {
        return boost::lexical_cast<T>(token);
    } else {
        static_assert(detail::always_false_v<T>,
                      "from_token has no conversion for this column type. "
                      "Add one to command_token.hpp before generating a shell "
                      "command unit for an entity that uses it.");
    }
}

/**
 * @brief Convert one command token, naming the column on failure.
 *
 * The generated command units pass the column name so a malformed token
 * reports the field it belongs to. Without it the caller sees only the
 * boost::bad_lexical_cast text, which names no field.
 */
template <typename T>
T from_token(const std::string& token, std::string_view column_name) {
    try {
        return from_token<T>(token);
    } catch (const std::exception&) {
        throw std::runtime_error("Invalid value for " + std::string(column_name) + ": " + token);
    }
}

}

#endif
