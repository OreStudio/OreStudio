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
#ifndef ORES_ORE_CORE_XML_SCANNER_HPP
#define ORES_ORE_CORE_XML_SCANNER_HPP

#include "ores.ore.core/export.hpp"
#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::ore::xml {

/**
 * @brief What the scanner found next.
 */
enum class event_type : std::uint8_t {
    begin_element,
    end_element,
    attribute,
    text,
    end_of_document,
    error,
};

/**
 * @brief One thing the scanner found.
 *
 * The name and the value are views into the document buffer. They stay valid
 * until the next call, because the scratch buffer that holds a decoded value
 * is reused.
 */
struct event {
    event_type kind = event_type::error;
    std::string_view name;
    std::string_view value;
};

/**
 * @brief A zero-copy pull reader for the XML that ORE documents use.
 *
 * Every value is a view into the document buffer. The only bytes the reader
 * writes are the decoded text of a value that carries an entity reference,
 * and those go into one reused scratch buffer reserved once per document, so
 * the reader allocates nothing per event.
 *
 * Contract: every begin_element is followed by exactly one end_element. A
 * self-closing tag yields the pair with nothing between them. Attributes
 * arrive as attribute events between begin_element and the first child.
 *
 * The accepted subset is stated rather than assumed: elements, attributes,
 * text, comments, processing instructions, CDATA sections and the five
 * predefined entities. A declaration, a depth over the limit, or a mismatched
 * end tag is rejected with a line and a column.
 */
class ORES_ORE_CORE_EXPORT reader {
public:
    explicit reader(std::string_view document);

    /**
     * @brief Reads the next event.
     */
    auto next() -> event;

    [[nodiscard]] auto failed() const -> bool {
        return error_position_ != no_error;
    }
    [[nodiscard]] auto error() const -> std::string_view {
        return error_message_;
    }
    [[nodiscard]] auto line() const -> std::uint32_t;
    [[nodiscard]] auto column() const -> std::uint32_t;

private:
    enum class state : std::uint8_t { content, head };

    static constexpr std::uint32_t no_error = 0xFFFFFFFFu;

    auto fail(std::string_view message) -> event;
    auto take_name() -> std::string_view;
    auto take_quoted() -> std::optional<std::string_view>;
    auto decode(std::string_view raw) -> std::string_view;

    static constexpr std::uint8_t max_depth = 64;

    std::string_view document_;
    std::array<std::string_view, max_depth> open_{};
    std::string scratch_;
    std::string_view error_message_;
    std::uint32_t position_ = 0;
    std::uint32_t error_position_ = no_error;
    std::uint8_t depth_ = 0;
    state state_ = state::content;
};

}

#endif
