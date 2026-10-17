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
#ifndef ORES_ORE_CORE_XML_DOCUMENT_HPP
#define ORES_ORE_CORE_XML_DOCUMENT_HPP

#include "ores.ore.core/export.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief One entry in the document's token tape.
 *
 * Names and values are offsets into the document's own buffer, so a token is
 * sixteen bytes and building one copies no text. This is the transient
 * representation the guidelines allow at the boundary: it holds the document
 * and nothing that a reader could mistake for a domain object.
 */
enum class token_kind : std::uint8_t {
    begin_element,
    end_element,
    attribute,
    text,
};

struct token {
    std::uint32_t name_begin = 0;
    std::uint32_t value_begin = 0;
    std::uint32_t name_length = 0;
    std::uint32_t value_length = 0;
    token_kind kind = token_kind::text;
    // Set when the value sits in the decoded-text arena rather than in the
    // document buffer. Only values carrying an entity reference need it, so a
    // document with none appends nothing.
    bool value_decoded = false;
};

/**
 * @brief Where a document failed to parse.
 */
struct parse_error {
    std::string message;
    std::uint32_t line = 0;
    std::uint32_t column = 0;
};

/**
 * @brief One XML document, held as an ordered tape of elements, attributes and
 * text.
 *
 * The tape keeps the document's order, so the tape is a valid document to
 * write back without consulting a schema. It keeps no comments, no processing
 * instructions, no CDATA boundaries and no whitespace between elements,
 * because none of those carry data.
 */
class ORES_ORE_CORE_EXPORT document {
public:
    document() = default;

    /**
     * @brief Parses a document.
     *
     * The document owns a copy of the text, because the tape points into it.
     * One allocation for the text and one for the tape is the whole cost.
     */
    static auto read(std::string_view text, parse_error& error) -> std::optional<document>;

    [[nodiscard]] auto tokens() const -> const std::vector<token>& {
        return tokens_;
    }
    [[nodiscard]] auto name(const token& t) const -> std::string_view {
        return std::string_view(buffer_).substr(t.name_begin, t.name_length);
    }
    [[nodiscard]] auto value(const token& t) const -> std::string_view {
        const auto& from = t.value_decoded ? decoded_ : buffer_;
        return std::string_view(from).substr(t.value_begin, t.value_length);
    }
    [[nodiscard]] auto root() const -> std::string_view;

    /**
     * @brief Compares the data two documents carry.
     *
     * Whitespace-only text is ignored, so formatting differences do not
     * register. Everything else compares exactly, including the digits of
     * every number.
     */
    friend auto operator==(const document& lhs, const document& rhs) -> bool;
    friend auto operator!=(const document& lhs, const document& rhs) -> bool {
        return !(lhs == rhs);
    }

    /**
     * @brief Writes the document as indented XML.
     *
     * The output is appended to `out`, and no schema is consulted: the tape
     * already holds the document in its original order, so the order is what
     * made the original valid.
     */
    auto write(std::string& out) const -> void;

private:
    /**
     * @brief True when a text token holds formatting and no data.
     *
     * Whitespace between two pieces of markup is indentation. Whitespace that
     * is the whole content of an element is the element's value, and dropping
     * it would lose an empty string that the document states explicitly.
     */
    [[nodiscard]] auto is_formatting(std::size_t index) const -> bool;

    std::string buffer_;
    std::string decoded_;
    std::vector<token> tokens_;
};

/**
 * @brief True when the text carries no data, only formatting.
 */
auto is_blank(std::string_view text) -> bool;

}

#endif
