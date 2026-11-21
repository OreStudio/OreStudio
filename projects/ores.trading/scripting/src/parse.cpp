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
#include "ores.trading.scripting/parse.hpp"
#include "ores.trading.scripting/ore/ast_printer.hpp"
#include "ores.trading.scripting/ore/ast_to_script_converter.hpp"
#include "ores.trading.scripting/ore/random_ast_generator.hpp"
#include "ores.trading.scripting/ore/script_parser.hpp"

namespace ores::trading::scripting {

namespace {

/// The column width Spirit's position-tracking iterator gives a tab.
constexpr std::size_t spirit_tab_width = 4;

/// The text of a one-based line, without its line break.
std::string_view line_of(std::string_view text, std::size_t line) {
    std::size_t start = 0;
    for (std::size_t current = 1; current < line; ++current) {
        const auto next = text.find('\n', start);
        if (next == std::string_view::npos)
            return {};
        start = next + 1;
    }
    const auto end = text.find('\n', start);
    auto out =
        text.substr(start, end == std::string_view::npos ? text.size() - start : end - start);
    // A line ended by \r\n reads as one ended by \n.
    if (out.ends_with('\r'))
        out.remove_suffix(1);
    return out;
}

/*
 * Spirit counts a column in bytes and widens a tab to the next multiple of four;
 * a diagnostic counts code points, a tab as one. Walk the line as Spirit does
 * until its column is reached, counting the code points passed.
 */
std::size_t code_point_column(std::string_view line, std::size_t spirit_column) {
    std::size_t spirit = 1, code_points = 1;
    for (const char c : line) {
        if (spirit >= spirit_column)
            break;
        if (c == '\t')
            spirit += spirit_tab_width - (spirit - 1) % spirit_tab_width;
        else
            ++spirit;
        // A UTF-8 continuation byte belongs to the code point before it.
        if ((static_cast<unsigned char>(c) & 0xC0) != 0x80)
            ++code_points;
    }
    return code_points;
}

diagnostic syntax_error(std::string_view text, const LocationInfo& where, std::string message) {
    diagnostic d;
    d.severity = severity::error;
    d.code = "SCR1001";
    d.message = std::move(message);
    if (where.initialised) {
        const auto line = line_of(text, where.lineStart);
        d.start =
            position{.line = where.lineStart, .column = code_point_column(line, where.columnStart)};
    }
    return d;
}

}

parse_result parse(std::string_view text) {
    const std::string script(text);
    parse_result result;
    try {
        ScriptParser parser(script);
        if (parser.success()) {
            result.tree = parser.ast();
            return result;
        }
        const auto& error = parser.error();
        if (error.expectedWhere.initialised && !error.expectedWhat.empty())
            result.diagnostics.push_back(
                syntax_error(text, error.expectedWhere, "expected " + error.expectedWhat));
        else
            result.diagnostics.push_back(syntax_error(
                text, error.stoppedParsingAt, "the script cannot be parsed from here"));
    } catch (const script_error& e) {
        result.diagnostics.push_back(
            syntax_error(text,
                         LocationInfo(),
                         std::string("ORE's parser failed at an unknown position: ") + e.what()));
    }
    return result;
}

std::string ast_to_script(const ASTNodePtr& tree) {
    return to_script(tree);
}

std::string print_ast(const ASTNodePtr& tree, bool with_locations) {
    return to_string(tree, with_locations);
}

ASTNodePtr
generate_random_ast(std::size_t max_sequence_length, std::size_t max_depth, std::size_t seed) {
    return generateRandomAST(max_sequence_length, max_depth, seed);
}

}
