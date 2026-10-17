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
#include "ores.ore.core/xml/scanner.hpp"
#include <optional>
#include <string_view>

namespace ores::ore::xml {

namespace {

constexpr bool is_space(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

constexpr bool ends_name(char c) {
    return is_space(c) || c == '>' || c == '/' || c == '=' || c == '<';
}

auto skip_space(std::string_view text, std::uint32_t& i) -> void {
    while (i < text.size() && is_space(text[i]))
        ++i;
}

auto starts_with(std::string_view text, std::uint32_t at, std::string_view prefix) -> bool {
    return text.substr(at, prefix.size()) == prefix;
}

/**
 * @brief Skips a construct that opens at `at` and closes with the terminator.
 *
 * The search starts past the whole opener, not two bytes into it. ORE ships
 * documents whose comment opens as "<!-->" (an abrupt close that XML forbids
 * and every parser in use repairs), and starting the search inside the opener
 * would end the comment on its own first line and read the rest as markup.
 */
auto skip_bracketed(std::string_view text,
                    std::uint32_t at,
                    std::string_view opener,
                    std::string_view terminator) -> std::optional<std::uint32_t> {
    const auto close = text.find(terminator, at + opener.size());
    if (close == std::string_view::npos)
        return std::nullopt;
    return static_cast<std::uint32_t>(close + terminator.size());
}

}

reader::reader(std::string_view document)
    : document_(document) {
    scratch_.reserve(128);
}

auto reader::fail(std::string_view message) -> event {
    if (error_position_ == no_error) {
        error_position_ = position_;
        error_message_ = message;
    }
    return event{event_type::error, {}, {}};
}

auto reader::line() const -> std::uint32_t {
    const auto stop = failed() ? error_position_ : position_;
    std::uint32_t line = 1;
    for (std::uint32_t i = 0; i < stop && i < document_.size(); ++i)
        line += document_[i] == '\n' ? 1 : 0;
    return line;
}

auto reader::column() const -> std::uint32_t {
    const auto stop = failed() ? error_position_ : position_;
    std::uint32_t column = 1;
    for (std::uint32_t i = 0; i < stop && i < document_.size(); ++i)
        column = document_[i] == '\n' ? 1 : column + 1;
    return column;
}

auto reader::take_name() -> std::string_view {
    const auto start = position_;
    while (position_ < document_.size() && !ends_name(document_[position_]))
        ++position_;
    return document_.substr(start, position_ - start);
}

auto reader::take_quoted() -> std::optional<std::string_view> {
    if (position_ >= document_.size())
        return std::nullopt;
    const char quote = document_[position_];
    if (quote != '"' && quote != '\'')
        return std::nullopt;
    const auto close = document_.find(quote, position_ + 1);
    if (close == std::string_view::npos)
        return std::nullopt;
    const auto start = position_ + 1;
    position_ = static_cast<std::uint32_t>(close + 1);
    return document_.substr(start, close - start);
}

auto reader::decode(std::string_view raw) -> std::string_view {
    const auto amp = raw.find('&');
    if (amp == std::string_view::npos)
        return raw;

    scratch_.clear();
    scratch_.append(raw.substr(0, amp));
    std::uint32_t i = static_cast<std::uint32_t>(amp);
    while (i < raw.size()) {
        if (raw[i] != '&') {
            scratch_.push_back(raw[i++]);
            continue;
        }
        const auto semi = raw.find(';', i);
        if (semi == std::string_view::npos) {
            scratch_.push_back(raw[i++]);
            continue;
        }
        const auto entity = raw.substr(i + 1, semi - i - 1);
        if (entity == "amp")
            scratch_.push_back('&');
        else if (entity == "lt")
            scratch_.push_back('<');
        else if (entity == "gt")
            scratch_.push_back('>');
        else if (entity == "quot")
            scratch_.push_back('"');
        else if (entity == "apos")
            scratch_.push_back('\'');
        else
            // An entity this reader does not know is kept verbatim rather than
            // guessed at, so a document that uses one still parses and the
            // unknown text stays visible.
            scratch_.append(raw.substr(i, semi - i + 1));
        i = static_cast<std::uint32_t>(semi + 1);
    }
    return scratch_;
}

auto reader::next() -> event {
    if (failed())
        return event{event_type::error, {}, {}};

    for (;;) {
        if (state_ == state::head) {
            skip_space(document_, position_);
            if (position_ >= document_.size())
                return fail("unexpected end of document inside a tag");
            if (document_[position_] == '/') {
                if (!starts_with(document_, position_, "/>"))
                    return fail("expected '>' after '/'");
                position_ += 2;
                state_ = state::content;
                if (depth_ == 0)
                    return fail("a tag closed with no open element");
                return event{event_type::end_element, open_[--depth_], {}};
            }
            if (document_[position_] == '>') {
                ++position_;
                state_ = state::content;
                continue;
            }
            const auto name = take_name();
            if (name.empty())
                return fail("expected an attribute name");
            skip_space(document_, position_);
            if (position_ >= document_.size() || document_[position_] != '=')
                return fail("expected '=' after an attribute name");
            ++position_;
            skip_space(document_, position_);
            const auto raw = take_quoted();
            if (!raw)
                return fail("expected a quoted attribute value");
            return event{event_type::attribute, name, decode(*raw)};
        }

        // Comments and processing instructions are skipped before anything
        // else is dispatched. No whitespace is skipped with them: whitespace
        // before markup is a text node, even when it is only formatting, and
        // deciding what to do with it belongs to the caller that has the whole
        // document.
        for (;;) {
            if (position_ >= document_.size())
                return event{event_type::end_of_document, {}, {}};
            if (!starts_with(document_, position_, "<!--") &&
                !starts_with(document_, position_, "<?"))
                break;
            const bool comment = starts_with(document_, position_, "<!--");
            const auto opener = comment ? std::string_view("<!--") : std::string_view("<?");
            const auto terminator = comment ? std::string_view("-->") : std::string_view("?>");
            const auto skipped = skip_bracketed(document_, position_, opener, terminator);
            if (!skipped)
                return fail("unterminated comment or processing instruction");
            position_ = *skipped;
        }

        if (document_[position_] != '<') {
            const auto start = position_;
            const auto open = document_.find('<', start);
            position_ = open == std::string_view::npos ?
                            static_cast<std::uint32_t>(document_.size()) :
                            static_cast<std::uint32_t>(open);
            return event{event_type::text, {}, decode(document_.substr(start, position_ - start))};
        }

        if (starts_with(document_, position_, "<![CDATA[")) {
            const auto close = document_.find("]]>", position_ + 9);
            if (close == std::string_view::npos)
                return fail("unterminated CDATA section");
            const auto start = position_ + 9;
            position_ = static_cast<std::uint32_t>(close + 3);
            return event{event_type::text, {}, document_.substr(start, close - start)};
        }

        if (starts_with(document_, position_, "</")) {
            position_ += 2;
            const auto name = take_name();
            skip_space(document_, position_);
            if (position_ >= document_.size() || document_[position_] != '>')
                return fail("expected '>' after an element name");
            ++position_;
            if (depth_ == 0 || name != open_[depth_ - 1])
                return fail("end tag does not match the open element");
            --depth_;
            return event{event_type::end_element, name, {}};
        }

        if (starts_with(document_, position_, "<!"))
            return fail("declarations are not part of the document subset this reader accepts");

        ++position_;
        const auto opened = take_name();
        if (opened.empty())
            return fail("expected an element name");
        if (depth_ == max_depth)
            return fail("the document nests deeper than this reader accepts");
        open_[depth_++] = opened;
        state_ = state::head;
        return event{event_type::begin_element, opened, {}};
    }
}

}
