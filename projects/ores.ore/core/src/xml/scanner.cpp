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

constexpr bool ends_name(char c) {
    return detail::is_space(c) || c == '>' || c == '/' || c == '=' || c == '<';
}

auto skip_space(std::string_view text, std::uint32_t& i) -> void {
    while (i < text.size() && detail::is_space(text[i]))
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

namespace {

auto named_entity(std::string_view entity) -> const char* {
    if (entity == "amp")
        return "&";
    if (entity == "lt")
        return "<";
    if (entity == "gt")
        return ">";
    if (entity == "quot")
        return "\"";
    if (entity == "apos")
        return "'";
    return nullptr;
}

/**
 * @brief Reads a character reference such as "#65" or "#x41".
 *
 * Returns nothing for a spelling that is not a reference or that names a code
 * point XML does not allow, so the caller can keep the bytes verbatim instead
 * of inventing a character.
 */
auto character_reference(std::string_view entity) -> std::optional<char32_t> {
    if (entity.size() < 2 || entity[0] != '#')
        return std::nullopt;
    const bool hex = entity[1] == 'x' || entity[1] == 'X';
    const auto digits = entity.substr(hex ? 2 : 1);
    if (digits.empty())
        return std::nullopt;

    std::uint32_t code = 0;
    for (const char c : digits) {
        std::uint32_t digit = 0;
        if (c >= '0' && c <= '9')
            digit = static_cast<std::uint32_t>(c - '0');
        else if (hex && c >= 'a' && c <= 'f')
            digit = static_cast<std::uint32_t>(c - 'a' + 10);
        else if (hex && c >= 'A' && c <= 'F')
            digit = static_cast<std::uint32_t>(c - 'A' + 10);
        else
            return std::nullopt;
        if (code > 0x10FFFFu / 16u)
            return std::nullopt;
        code = code * (hex ? 16u : 10u) + digit;
        if (!hex && code > 0x10FFFFu)
            return std::nullopt;
    }

    const bool allowed = code == 0x9 || code == 0xA || code == 0xD ||
                         (code >= 0x20 && code <= 0xD7FF) || (code >= 0xE000 && code <= 0xFFFD) ||
                         (code >= 0x10000 && code <= 0x10FFFF);
    if (!allowed)
        return std::nullopt;
    return static_cast<char32_t>(code);
}

auto append_utf8(std::string& out, char32_t code) -> void {
    const auto c = static_cast<std::uint32_t>(code);
    if (c < 0x80) {
        out.push_back(static_cast<char>(c));
    } else if (c < 0x800) {
        out.push_back(static_cast<char>(0xC0 | (c >> 6)));
        out.push_back(static_cast<char>(0x80 | (c & 0x3F)));
    } else if (c < 0x10000) {
        out.push_back(static_cast<char>(0xE0 | (c >> 12)));
        out.push_back(static_cast<char>(0x80 | ((c >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (c & 0x3F)));
    } else {
        out.push_back(static_cast<char>(0xF0 | (c >> 18)));
        out.push_back(static_cast<char>(0x80 | ((c >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((c >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (c & 0x3F)));
    }
}

}

auto reader::normalise(std::string_view raw, bool attribute) -> std::string_view {
    // The common case is a value with nothing to normalise, and it used to pay
    // a byte-at-a-time scan of the whole value to find that out. A callgrind
    // profile put that scan at forty percent of the scanner's instructions.
    // find() for a single character is memchr, so the test is now a few
    // vectorised passes instead of one scalar one.
    const bool has_entity = raw.find('&') != std::string_view::npos;
    const bool has_carriage_return = raw.find('\r') != std::string_view::npos;
    bool has_attribute_space = false;
    if (attribute)
        has_attribute_space = has_carriage_return || raw.find('\t') != std::string_view::npos ||
                              raw.find('\n') != std::string_view::npos;
    if (!has_entity && !has_carriage_return && !has_attribute_space)
        return raw;

    scratch_.clear();
    for (std::size_t i = 0; i < raw.size();) {
        const char c = raw[i];
        if (c == '&') {
            const auto semi = raw.find(';', i);
            if (semi == std::string_view::npos) {
                scratch_.push_back(c);
                ++i;
                continue;
            }
            const auto entity = raw.substr(i + 1, semi - i - 1);
            if (const auto* named = named_entity(entity)) {
                scratch_.append(named);
            } else if (const auto code = character_reference(entity)) {
                append_utf8(scratch_, *code);
            } else {
                // An entity this reader does not know, or a reference to a
                // code point XML forbids, is kept verbatim rather than guessed
                // at, so the bytes stay visible instead of becoming a
                // character the document never named.
                scratch_.append(raw.substr(i, semi - i + 1));
            }
            i = semi + 1;
            continue;
        }
        if (attribute) {
            // A literal tab or line break in an attribute value is a space
            // once the document is normalised. A character reference to one
            // is not, and it survived the branch above.
            if (c == '\t' || c == '\n' || c == '\r') {
                scratch_.push_back(' ');
                ++i;
                continue;
            }
        } else if (c == '\r') {
            // Line endings are normalised before anything else sees the text.
            scratch_.push_back('\n');
            ++i;
            if (i < raw.size() && raw[i] == '\n')
                ++i;
            continue;
        }
        scratch_.push_back(c);
        ++i;
    }
    return std::string_view(scratch_);
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
            return event{event_type::attribute, name, normalise(*raw, true)};
        }

        // Comments and processing instructions are skipped before anything
        // else is dispatched. No whitespace is skipped with them: whitespace
        // before markup is a text node, even when it is only formatting, and
        // deciding what to do with it belongs to the caller that has the whole
        // document.
        for (;;) {
            if (position_ >= document_.size())
                return event{event_type::end_of_document, {}, {}};
            if (document_[position_] != '<')
                break;
            const char after = position_ + 1 < document_.size() ? document_[position_ + 1] : '\0';
            if (after != '!' && after != '?')
                break;
            // Only now is the longer prefix worth comparing. Testing the second
            // character first keeps the common tag path free of memcmp, which a
            // callgrind profile showed to be a tenth of the scanner.
            const bool comment = after == '!' && starts_with(document_, position_, "<!--");
            if (!comment && after != '?')
                break;
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
            return event{
                event_type::text, {}, normalise(document_.substr(start, position_ - start), false)};
        }

        const char after = position_ + 1 < document_.size() ? document_[position_ + 1] : '\0';

        if (after == '!') {
            if (starts_with(document_, position_, "<![CDATA[")) {
                const auto close = document_.find("]]>", position_ + 9);
                if (close == std::string_view::npos)
                    return fail("unterminated CDATA section");
                const auto start = position_ + 9;
                position_ = static_cast<std::uint32_t>(close + 3);
                return event{event_type::text, {}, document_.substr(start, close - start)};
            }
            return fail("declarations are not part of the document subset this reader accepts");
        }

        if (after == '/') {
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
