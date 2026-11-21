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
#ifndef ORES_TRADING_SCRIPTING_SRC_SPIRIT_COLUMNS_HPP
#define ORES_TRADING_SCRIPTING_SRC_SPIRIT_COLUMNS_HPP

#include <cstddef>
#include <optional>
#include <string_view>

/**
 * @file spirit_columns.hpp
 * @brief Positions as Spirit's position-tracking iterator counts them, read
 * back into a script's text.
 *
 * Spirit counts a column in bytes and widens a tab to the next multiple of four.
 * A diagnostic counts code points, a tab as one, and a rule is sliced from the
 * text by byte offset; both are read here, once.
 */
namespace ores::trading::scripting::spirit_columns {

/// The column width Spirit gives a tab.
inline constexpr std::size_t tab_width = 4;

/// Spirit's column after one more character of a line.
inline std::size_t advance(std::size_t column, char c) {
    return c == '\t' ? column + tab_width - (column - 1) % tab_width : column + 1;
}

/// The text of a one-based line, without its line break; a CRLF reads as LF.
/// Nothing for a line past the text's end.
inline std::optional<std::string_view> line_of(std::string_view text, std::size_t line) {
    std::size_t start = 0;
    for (std::size_t current = 1; current < line; ++current) {
        const auto next = text.find('\n', start);
        if (next == std::string_view::npos)
            return std::nullopt;
        start = next + 1;
    }
    const auto end = text.find('\n', start);
    auto out =
        text.substr(start, end == std::string_view::npos ? text.size() - start : end - start);
    if (out.ends_with('\r'))
        out.remove_suffix(1);
    return out;
}

/// The code point column of a Spirit column of a line.
inline std::size_t code_point_column(std::string_view line, std::size_t spirit_column) {
    std::size_t spirit = 1, code_points = 1;
    for (const char c : line) {
        if (spirit >= spirit_column)
            break;
        spirit = advance(spirit, c);
        // A UTF-8 continuation byte belongs to the code point before it.
        if ((static_cast<unsigned char>(c) & 0xC0) != 0x80)
            ++code_points;
    }
    return code_points;
}

/// The byte offset in the text of a one-based line and a Spirit column.
inline std::size_t byte_offset(std::string_view text, std::size_t line, std::size_t spirit_column) {
    const auto row = line_of(text, line);
    if (!row)
        return text.size();
    std::size_t at = static_cast<std::size_t>(row->data() - text.data());
    for (std::size_t column = 1; column < spirit_column && at < text.size() && text[at] != '\n';
         ++at)
        column = advance(column, text[at]);
    return at;
}

}

#endif
