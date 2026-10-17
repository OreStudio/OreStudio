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
#pragma once

// A zero-copy pull reader for the subset of XML that ORE documents use.
//
// Every value is a view into the document buffer. The only bytes the reader
// writes are the decoded text of a value carrying an entity reference, and
// those go into one reused scratch buffer reserved once per document. The
// reader allocates nothing per event.
//
// Contract: every begin_element is followed by exactly one end_element. A
// self-closing tag yields the pair with nothing between them. Attributes
// arrive as attribute events between begin_element and the first child.

#include <array>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ore::xml {

enum class event_type : std::uint8_t {
    begin_element,
    end_element,
    attribute,
    text,
    end_of_document,
    error,
};

struct event {
    event_type kind = event_type::error;
    std::string_view name;
    std::string_view value;
};

class reader {
public:
    explicit reader(std::string_view document);

    // Views stay valid until the next call, because the scratch buffer that
    // holds decoded text is reused.
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
    std::uint8_t depth_ = 0;
    std::string scratch_;
    std::string_view error_message_;
    std::uint32_t position_ = 0;
    std::uint32_t error_position_ = no_error;
    state state_ = state::content;
};

} // namespace ore::xml
