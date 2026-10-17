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
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/scanner.hpp"

namespace ores::ore::xml {

namespace {

constexpr std::string_view indent_unit = "  ";

constexpr bool is_space(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

/**
 * @brief Appends the escaped form of text content.
 */
auto append_text(std::string& out, std::string_view text) -> void {
    for (const char c : text) {
        switch (c) {
            case '&':
                out.append("&amp;");
                break;
            case '<':
                out.append("&lt;");
                break;
            case '>':
                out.append("&gt;");
                break;
            // A carriage return in content is normalised away by every parser,
            // so it survives only as a character reference.
            case '\r':
                out.append("&#13;");
                break;
            default:
                out.push_back(c);
                break;
        }
    }
}

/**
 * @brief Appends the escaped form of an attribute value.
 *
 * Tabs and newlines become character references because attribute-value
 * normalisation would otherwise turn them into spaces and lose them.
 */
auto append_attribute(std::string& out, std::string_view value) -> void {
    for (const char c : value) {
        switch (c) {
            case '&':
                out.append("&amp;");
                break;
            case '<':
                out.append("&lt;");
                break;
            case '>':
                out.append("&gt;");
                break;
            case '"':
                out.append("&quot;");
                break;
            case '\t':
                out.append("&#9;");
                break;
            case '\n':
                out.append("&#10;");
                break;
            case '\r':
                out.append("&#13;");
                break;
            default:
                out.push_back(c);
                break;
        }
    }
}

}

auto is_blank(std::string_view text) -> bool {
    for (const char c : text)
        if (!is_space(c))
            return false;
    return true;
}

auto document::read(std::string_view text, parse_error& error) -> std::optional<document> {
    document result;
    result.buffer_.assign(text);

    reader input(result.buffer_);
    auto& tokens = result.tokens_;
    tokens.reserve(text.size() / 8 + 8);

    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::error) {
            error.message = std::string(input.error());
            error.line = input.line();
            error.column = input.column();
            return std::nullopt;
        }
        if (ev.kind == event_type::end_of_document)
            break;

        token entry;
        switch (ev.kind) {
            case event_type::begin_element:
                entry.kind = token_kind::begin_element;
                break;
            case event_type::end_element:
                entry.kind = token_kind::end_element;
                break;
            case event_type::attribute:
                entry.kind = token_kind::attribute;
                break;
            case event_type::text:
                entry.kind = token_kind::text;
                break;
            default:
                continue;
        }

        if (!ev.name.empty()) {
            entry.name_begin = static_cast<std::uint32_t>(ev.name.data() - result.buffer_.data());
            entry.name_length = static_cast<std::uint32_t>(ev.name.size());
        }
        if (!ev.value.empty()) {
            const auto* begin = ev.value.data();
            const bool decoded = begin < result.buffer_.data() ||
                                 begin >= result.buffer_.data() + result.buffer_.size();
            if (decoded) {
                entry.value_begin = static_cast<std::uint32_t>(result.decoded_.size());
                result.decoded_.append(ev.value);
                entry.value_decoded = true;
            } else {
                entry.value_begin = static_cast<std::uint32_t>(begin - result.buffer_.data());
            }
            entry.value_length = static_cast<std::uint32_t>(ev.value.size());
        }
        tokens.push_back(entry);
    }

    return result;
}

auto document::is_formatting(std::size_t index) const -> bool {
    const auto& t = tokens_[index];
    if (t.kind != token_kind::text || !is_blank(value(t)))
        return false;
    const bool opens_a_run = index == 0 || tokens_[index - 1].kind == token_kind::begin_element ||
                             tokens_[index - 1].kind == token_kind::attribute;
    const bool closes_the_run =
        index + 1 >= tokens_.size() || tokens_[index + 1].kind == token_kind::end_element;
    return !(opens_a_run && closes_the_run);
}

auto document::root() const -> std::string_view {
    for (const auto& t : tokens_)
        if (t.kind == token_kind::begin_element)
            return name(t);
    return {};
}

auto operator==(const document& lhs, const document& rhs) -> bool {
    auto left = lhs.tokens_.begin();
    auto right = rhs.tokens_.begin();

    auto skip_blank = [](auto& it, auto& owner, const auto& end) {
        while (it != end &&
               owner.is_formatting(static_cast<std::size_t>(it - owner.tokens_.begin())))
            ++it;
    };

    for (;;) {
        skip_blank(left, lhs, lhs.tokens_.end());
        skip_blank(right, rhs, rhs.tokens_.end());
        const bool left_done = left == lhs.tokens_.end();
        const bool right_done = right == rhs.tokens_.end();
        if (left_done || right_done)
            return left_done == right_done;
        if (left->kind != right->kind)
            return false;
        if (left->kind == token_kind::end_element) {
            ++left;
            ++right;
            continue;
        }
        if (lhs.name(*left) != rhs.name(*right))
            return false;
        if (lhs.value(*left) != rhs.value(*right))
            return false;
        ++left;
        ++right;
    }
}

auto document::write(std::string& out) const -> void {
    // An element whose content mixes text with children cannot be indented,
    // because the indentation would become part of its text. ORE documents
    // are not written that way, but the writer must not depend on that, so a
    // first pass marks such elements and the second pass leaves them alone.
    std::vector<bool> mixed(tokens_.size(), false);
    {
        std::vector<std::size_t> open;
        for (std::size_t i = 0; i < tokens_.size(); ++i) {
            const auto& t = tokens_[i];
            if (t.kind == token_kind::begin_element) {
                open.push_back(i);
            } else if (t.kind == token_kind::end_element) {
                if (!open.empty())
                    open.pop_back();
            } else if (t.kind == token_kind::text && !open.empty() && !is_formatting(i)) {
                mixed[open.back()] = true;
            }
        }
    }

    auto indent = [&out](std::size_t depth) {
        if (!out.empty())
            out.push_back('\n');
        for (std::size_t i = 0; i < depth; ++i)
            out.append(indent_unit);
    };

    std::size_t depth = 0;
    bool tag_open = false;
    std::vector<bool> open_mixed;

    auto close_tag = [&out, &tag_open] {
        if (tag_open) {
            out.push_back('>');
            tag_open = false;
        }
    };

    for (std::size_t i = 0; i < tokens_.size(); ++i) {
        const auto& t = tokens_[i];
        switch (t.kind) {
            case token_kind::begin_element: {
                close_tag();
                const bool indenting = open_mixed.empty() || !open_mixed.back();
                if (indenting)
                    indent(depth);
                out.push_back('<');
                out.append(name(t));
                tag_open = true;
                open_mixed.push_back(mixed[i]);
                ++depth;
                continue;
            }
            case token_kind::attribute: {
                out.push_back(' ');
                out.append(name(t)).append("=\"");
                append_attribute(out, value(t));
                out.push_back('"');
                continue;
            }
            case token_kind::text: {
                if (is_formatting(i))
                    continue;
                close_tag();
                append_text(out, value(t));
                continue;
            }
            case token_kind::end_element: {
                --depth;
                const bool closing_mixed = !open_mixed.empty() && open_mixed.back();
                if (!open_mixed.empty())
                    open_mixed.pop_back();
                if (tag_open) {
                    out.append("/>");
                    tag_open = false;
                } else {
                    if (!closing_mixed)
                        indent(depth);
                    out.append("</").append(name(t)).push_back('>');
                }
                continue;
            }
        }
    }
    if (!out.empty() && out.back() != '\n')
        out.push_back('\n');
}

}
