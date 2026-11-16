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
#ifndef ORES_HTTP_ROUTES_REFDATA_CURVE_DOCUMENT_CELLS_HPP
#define ORES_HTTP_ROUTES_REFDATA_CURVE_DOCUMENT_CELLS_HPP

#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <charconv>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::refdata::document {

namespace codec = ores::utility::codec;

/**
 * @brief One cell of a row: the value the wire carries, or nothing when it
 * carries none.
 *
 * The screen states a leaf's value as text or as nothing, and a column the row
 * does not carry is not the same as one it carries empty, so the absence is kept.
 */
using cell = std::optional<std::string>;

/// One row of the document, by the column the entity names it by.
using row = std::map<std::string, cell, std::less<>>;

/**
 * @brief The curve configuration document, flattened into the rows the screen
 * joins.
 *
 * The document arrives typed, so a row's columns are its members. The wiring
 * names a column as text, and the screen joins that column to the contract entry
 * of the leaf it holds. This is where the two meet: every row of every vector
 * becomes cells the wiring reads by name, and a column a row does not carry stays
 * absent rather than becoming empty text.
 */
class cells final {
public:
    explicit cells(const ores::refdata::messaging::curve_configuration_document& document);

    /// The header row, which is the document's own configuration row.
    [[nodiscard]] const row& header() const noexcept {
        return header_;
    }

    /**
     * @brief The rows of one document vector, or nothing for a name this
     * document does not carry.
     */
    [[nodiscard]] const std::vector<row>* find(std::string_view vector) const;

    /// The rows of one vector, empty for a name the document does not carry.
    [[nodiscard]] const std::vector<row>& at(std::string_view vector) const;

private:
    row header_;
    std::map<std::string, std::vector<row>, std::less<>> rows_;
};

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
 * ORE writes one; a number is its shortest form, so a value the screen reads and
 * writes back does not drift; and a type with a stated wire text, such as a uuid
 * or an instant, is that text. A member that may be absent is absent rather than
 * empty, because the screen states the two differently.
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

/// The cell one column of one row holds, or nothing for a column the row does not carry.
[[nodiscard]] cell cell_at(const row& source, std::string_view column);

/// The text of one column of one row, empty for a column the row does not carry.
[[nodiscard]] std::string text_of(const row& source, std::string_view column);

/// The number one column states, zero for a column that states none.
[[nodiscard]] int number_of(const row& source, std::string_view column);

}

#endif
