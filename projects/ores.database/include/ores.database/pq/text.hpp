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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_PQ_TEXT_HPP
#define ORES_DATABASE_PQ_TEXT_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/codec.hpp"
#include "ores.database/repository/db_types.hpp"
#include <charconv>
#include <concepts>
#include <optional>
#include <string>
#include <string_view>

/*
 * The text form of a value: the SQL an entity model does not generate passes
 * and reads it, where the server types the value from where it is used. A
 * generated repository holds the type its column has and binds and reads it in
 * the binary format (see codec.hpp), except for a column it keeps as text,
 * which this header's to_text and from_text also serve.
 */
namespace ores::database::pq {

[[noreturn]] ORES_DATABASE_EXPORT void unreadable_text(std::string_view text,
                                                       std::string_view type);

/**
 * @brief Reads a timestamptz in the text format, the bitemporal never as the
 * clock's maximum.
 */
ORES_DATABASE_EXPORT repository::db_timestamp parse_timestamp_text(std::string_view text);

inline param text_param(std::string text) {
    return {.bytes = std::move(text), .text = true};
}

inline param to_text(const std::string& v) {
    return text_param(v);
}

inline param to_text(bool v) {
    return text_param(v ? "true" : "false");
}

template <std::integral T>
    requires(!std::same_as<T, bool>)
param to_text(T v) {
    return text_param(std::to_string(v));
}

ORES_DATABASE_EXPORT param to_text(double v);

/**
 * @brief The instant in the text format, the clock's maximum as the
 * bitemporal never the database stores.
 */
ORES_DATABASE_EXPORT param to_text(const repository::db_timestamp& v);

template <typename T>
param to_text(const std::optional<T>& v) {
    if (!v)
        return {.null = true, .text = true};
    return to_text(*v);
}

/**
 * @brief Reads one value of a text result into the type an entity holds.
 */
template <typename T>
T from_text(const result& r, int row, int column);

namespace detail {

template <typename T>
T parse_text(std::string_view text) {
    if constexpr (std::same_as<T, std::string>) {
        return std::string(text);
    } else if constexpr (std::same_as<T, bool>) {
        if (text == "t" || text == "true")
            return true;
        if (text == "f" || text == "false")
            return false;
        unreadable_text(text, "bool");
    } else if constexpr (std::integral<T> || std::floating_point<T>) {
        T v{};
        const auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), v);
        if (ec != std::errc{} || end != text.data() + text.size())
            unreadable_text(text, "number");
        return v;
    } else if constexpr (std::same_as<T, repository::db_timestamp>) {
        return parse_timestamp_text(text);
    } else {
        static_assert(sizeof(T) == 0, "No text form for this entity type");
    }
}

template <typename T>
struct text_reader {
    static T read(const result& r, int row, int column) {
        if (r.is_null(row, column))
            unexpected_null(row, column);
        return parse_text<T>(r.text(row, column));
    }
};

template <typename T>
struct text_reader<std::optional<T>> {
    static std::optional<T> read(const result& r, int row, int column) {
        if (r.is_null(row, column))
            return std::nullopt;
        return parse_text<T>(r.text(row, column));
    }
};

}

template <typename T>
T from_text(const result& r, int row, int column) {
    return detail::text_reader<T>::read(r, row, column);
}

}

#endif
