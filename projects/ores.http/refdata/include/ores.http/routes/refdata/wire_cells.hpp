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
#ifndef ORES_HTTP_ROUTES_REFDATA_WIRE_CELLS_HPP
#define ORES_HTTP_ROUTES_REFDATA_WIRE_CELLS_HPP

#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <charconv>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::http::routes::refdata::document {

namespace codec = ores::utility::codec;

/**
 * @brief One cell of a row: the value the wire carries, or nothing when it
 * carries none.
 *
 * A screen states a value as text or as nothing, and a column a row does not
 * carry is not the same as one it carries empty, so the absence is kept.
 */
using cell = std::optional<std::string>;

/// One row of a document, by the column an entity names it by.
using row = std::map<std::string, cell, std::less<>>;

/// Whether the type states the text its wire form takes.
template <typename T>
concept states_text = requires(const T& value) { codec::text_form<T>::to_text(value); };

/// The shortest text a number takes, so a value read back is the one written.
inline std::string shortest_text(double value) {
    char buffer[32];
    const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
    return std::string(buffer, written.ptr);
}

/**
 * @brief The text one cell of a row holds.
 *
 * A member the wire carries as a string is its text; a flag is written the way
 * ORE writes one; a number is its shortest form, so a value a screen reads and
 * writes back does not drift; and a type with a stated wire text, such as a uuid
 * or an instant, is that text. A member that may be absent is absent rather than
 * empty, because a screen states the two differently.
 */
template <typename T>
cell cell_of(const T& value) {
    if constexpr (codec::is_optional_member<T>) {
        return value ? cell_of(*value) : cell();
    } else if constexpr (std::is_same_v<T, bool>) {
        return value ? "true" : "false";
    } else if constexpr (std::is_integral_v<T>) {
        return std::to_string(value);
    } else if constexpr (std::is_floating_point_v<T>) {
        return shortest_text(static_cast<double>(value));
    } else if constexpr (states_text<T>) {
        return codec::text_form<T>::to_text(value);
    } else {
        static_assert(states_text<T>, "a row member with no wire text cannot be read");
    }
}

/// The columns of one row, as the cells a screen reads by name.
template <typename T>
row row_of(const T& source) {
    row one;
    codec::for_each_field(source, [&one](std::string_view name, const auto& value) {
        one.emplace(std::string(name), cell_of(value));
    });
    return one;
}

/**
 * @brief The cell one column of one row holds, wherever the row groups it.
 *
 * A row whose entity is grouped onto field groups carries its columns one level
 * down, under the group's name. A screen names a column by its own name and not
 * by its group, so the groups are searched when the row has no such member.
 */
template <typename T>
cell find_cell(const T& source, std::string_view column) {
    cell found;
    bool stated = false;
    codec::for_each_field(source, [&](std::string_view name, const auto& value) {
        if (stated)
            return;
        if constexpr (codec::states_wire_form<std::remove_cvref_t<decltype(value)>>) {
            // A group is not a column: the columns it holds are named by their
            // own names, so the group is searched and never read as one.
            auto nested = find_cell(value, column);
            if (nested) {
                found = nested;
                stated = true;
            }
        } else {
            if (name == column) {
                found = cell_of(value);
                stated = true;
            }
        }
    });
    return found;
}

/// The number one text states, or a refusal naming the column it did not.
template <typename T>
T number_of_text(const std::string& stated, std::string_view column) {
    T number{};
    const auto read = std::from_chars(stated.data(), stated.data() + stated.size(), number);
    if (read.ec != std::errc())
        throw std::invalid_argument("The column \"" + std::string(column) + "\" holds \"" + stated +
                                    "\", which is not a number.");
    return number;
}

/**
 * @brief Writes one cell into one member, in the member's own type.
 *
 * An absent cell leaves an optional empty, which is what a screen means by
 * unsetting a value, and it leaves the other kinds empty rather than refusing: a
 * column the caller states as nothing is a column with no value. A text the
 * member's type does not make is a refusal.
 *
 * @throws std::invalid_argument when a text is not a value the member takes.
 */
template <typename T>
void assign_cell(T& member, std::string_view column, const cell& value) {
    if constexpr (codec::is_optional_member<T>) {
        using inner = typename T::value_type;
        if (!value) {
            member.reset();
            return;
        }
        inner written{};
        assign_cell(written, column, value);
        member = std::move(written);
    } else if constexpr (std::is_same_v<T, bool>) {
        member = value.value_or(std::string()) == "true";
    } else if constexpr (std::is_integral_v<T>) {
        member = value ? number_of_text<T>(*value, column) : T{};
    } else if constexpr (std::is_floating_point_v<T>) {
        member = value ? static_cast<T>(number_of_text<double>(*value, column)) : T{};
    } else if constexpr (std::is_same_v<T, std::string>) {
        member = value.value_or(std::string());
    } else if constexpr (states_text<T>) {
        // A type with a stated text, such as a uuid or an instant, need not have
        // an empty value of its own, so an absent cell leaves it as it was.
        if (value)
            member = codec::text_form<T>::from_text(*value);
    } else {
        static_assert(states_text<T>, "a row member with no wire text cannot be written");
    }
}

/**
 * @brief Writes one column of one row, wherever the row groups it, and answers
 * whether the row carries that column.
 */
template <typename T>
bool set_cell(T& source, std::string_view column, const cell& value) {
    bool written = false;
    codec::for_each_field(source, [&](std::string_view name, auto& member) {
        if (written)
            return;
        if constexpr (codec::states_wire_form<std::remove_cvref_t<decltype(member)>>) {
            written = set_cell(member, column, value);
        } else {
            if (name == column) {
                assign_cell(member, column, value);
                written = true;
            }
        }
    });
    return written;
}

/// The cell one column of one row holds, or nothing for a column the row does not carry.
[[nodiscard]] cell cell_at(const row& source, std::string_view column);

/// The text of one column of one row, empty for a column the row does not carry.
[[nodiscard]] std::string text_of(const row& source, std::string_view column);

/// The number one column states, zero for a column that states none.
[[nodiscard]] int number_of(const row& source, std::string_view column);

}

#endif
