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
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <algorithm>
#include <cstdint>

namespace ores::ore::xml {

namespace {

/**
 * @brief The value a text states, with a number reduced to its decimal.
 *
 * A document may spell one number two ways and the schema is the only thing
 * that says which is allowed. Reading the text as a decimal when it is one
 * makes the two spellings compare equal, and leaves every other text -- a
 * date, a currency, a count of days -- exactly as it stands.
 */
auto canonical(std::string_view text) -> std::string {
    const auto parsed = utility::decimal::decimal::from_string(text);
    if (parsed)
        return parsed->to_string();
    return std::string(text);
}

auto join(const std::vector<std::string>& path, std::string_view leaf) -> std::string {
    std::string out;
    for (const auto& part : path) {
        if (!out.empty())
            out.push_back('/');
        out.append(part);
    }
    if (!leaf.empty()) {
        if (!out.empty())
            out.push_back('/');
        out.append(leaf);
    }
    return out;
}

}

auto value_fingerprint(const document& doc) -> std::vector<value_entry> {
    std::vector<value_entry> values;
    std::vector<std::string> path;
    std::string text;
    // An element that holds a child element cannot also be a leaf, so the
    // walk remembers whether one arrived and records its own text as mixed
    // content rather than as the element's value.
    std::vector<bool> has_child;

    for (const auto& token : doc.tokens()) {
        switch (token.kind) {
            case token_kind::begin_element:
                path.emplace_back(doc.name(token));
                has_child.push_back(false);
                text.clear();
                break;
            case token_kind::attribute:
                values.push_back(
                    {join(path, "@" + std::string(doc.name(token))), canonical(doc.value(token))});
                break;
            case token_kind::text:
                text.append(doc.value(token));
                break;
            case token_kind::end_element: {
                // Text that is the whole content of an element is that
                // element's value even when it is empty, because the schema
                // may spell a value that way. Text beside a child element is
                // data only when it is not formatting.
                const bool leaf = !has_child.empty() && !has_child.back();
                if (leaf) {
                    // Whitespace alone is formatting rather than data: a
                    // document writes an empty list as an element with a line
                    // break in it, and the value is the empty string.
                    values.push_back(
                        {join(path, {}), is_blank(text) ? std::string{} : canonical(text)});
                } else if (!is_blank(text)) {
                    values.push_back({join(path, "text()"), canonical(text)});
                }
                text.clear();
                if (!path.empty())
                    path.pop_back();
                if (!has_child.empty())
                    has_child.pop_back();
                if (!has_child.empty())
                    has_child.back() = true;
                break;
            }
        }
    }

    std::sort(values.begin(), values.end(), [](const value_entry& lhs, const value_entry& rhs) {
        if (lhs.path != rhs.path)
            return lhs.path < rhs.path;
        return lhs.value < rhs.value;
    });
    return values;
}

}
