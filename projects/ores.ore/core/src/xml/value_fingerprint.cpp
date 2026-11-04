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
#include "ores.ore.core/xml/lexical.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <string>
#include <string_view>
#include <utility>

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
    // A number may be written on its own line, and its lexical space allows
    // the space around it. A text that parses as a decimal is compared as the
    // number, so the reading's trim of it does not read as a difference. The
    // tape does not say which kind a leaf is, so a text value that happens to
    // parse as a number is compared the same way; the record comparison is the
    // stricter check for those.
    const auto parsed = utility::decimal::decimal::from_string(trim_space(text));
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

/**
 * @brief Walks a run of tokens and records the values they state.
 *
 * The walker is fed one token at a time, so a caller can walk a whole document
 * or switch the list it records into as it meets the elements it cares about.
 */
class value_walker {
public:
    explicit value_walker(const document& doc)
        : doc_(doc) {}

    /**
     * @brief Records what a token states.
     *
     * @param label The name the path gives a begin token, when it is not the
     * element's own name: a repeated element is labelled with its position.
     */
    auto feed(const token& t, std::vector<value_entry>& values, std::string_view label = {})
        -> void {
        switch (t.kind) {
            case token_kind::begin_element:
                path_.emplace_back(label.empty() ? doc_.name(t) : label);
                has_child_.push_back(false);
                text_.clear();
                break;
            case token_kind::attribute:
                values.push_back(
                    {join(path_, "@" + std::string(doc_.name(t))), canonical(doc_.value(t))});
                break;
            case token_kind::text:
                text_.append(doc_.value(t));
                break;
            case token_kind::end_element: {
                // Text that is the whole content of an element is that
                // element's value. An element with no text, or with only the
                // whitespace a document formats an empty list with, states no
                // value: ORE reads <FirstDate/> as it reads no FirstDate, so
                // the two are not a difference. Text beside a child element
                // is data only when it is not formatting.
                const bool leaf = !has_child_.empty() && !has_child_.back();
                if (leaf) {
                    if (!is_blank(text_))
                        values.push_back({join(path_, {}), canonical(text_)});
                } else if (!is_blank(text_)) {
                    values.push_back({join(path_, "text()"), canonical(text_)});
                }
                text_.clear();
                if (!path_.empty())
                    path_.pop_back();
                if (!has_child_.empty())
                    has_child_.pop_back();
                if (!has_child_.empty())
                    has_child_.back() = true;
                break;
            }
        }
    }

    [[nodiscard]] auto depth() const -> std::size_t {
        return path_.size();
    }

private:
    const document& doc_;
    std::vector<std::string> path_;
    std::string text_;
    // An element that holds a child element cannot also be a leaf, so the
    // walk remembers whether one arrived and records its own text as mixed
    // content rather than as the element's value.
    std::vector<bool> has_child_;
};

/**
 * @brief The label of each begin token in a run that repeats among its
 * siblings, keyed by the token's position: the name and its position, from one.
 *
 * A sequence such as the legs of a swap states the same element more than
 * once, and a value of the second leg is not a value of the first. Labelling
 * the repeats keeps them apart, so a value one leg lost is not read as a value
 * the other changed.
 */
auto sibling_labels(const document& doc, std::size_t from, std::size_t to)
    -> std::map<std::size_t, std::string> {
    const auto& tokens = doc.tokens();
    // The parent of each begin token, by the parent's position; the run's
    // first element has none.
    std::map<std::size_t, std::size_t> parent_of;
    std::vector<std::size_t> open;
    std::map<std::pair<std::size_t, std::string_view>, std::size_t> totals;
    constexpr std::size_t no_parent = static_cast<std::size_t>(-1);
    for (auto at = from; at < to; ++at) {
        const auto& t = tokens[at];
        if (t.kind == token_kind::begin_element) {
            const auto parent = open.empty() ? no_parent : open.back();
            parent_of[at] = parent;
            ++totals[{parent, doc.name(t)}];
            open.push_back(at);
        } else if (t.kind == token_kind::end_element && !open.empty()) {
            open.pop_back();
        }
    }

    std::map<std::size_t, std::string> labels;
    std::map<std::pair<std::size_t, std::string_view>, std::size_t> seen;
    for (const auto& [at, parent] : parent_of) {
        const auto name = doc.name(tokens[at]);
        if (totals[{parent, name}] < 2)
            continue;
        const auto position = ++seen[{parent, name}];
        labels[at] = std::string(name) + "[" + std::to_string(position) + "]";
    }
    return labels;
}

auto sorted(std::vector<value_entry> values) -> std::vector<value_entry> {
    std::sort(values.begin(), values.end(), [](const value_entry& lhs, const value_entry& rhs) {
        if (lhs.path != rhs.path)
            return lhs.path < rhs.path;
        return lhs.value < rhs.value;
    });
    return values;
}

}

auto value_fingerprint(const document& doc) -> std::vector<value_entry> {
    std::vector<value_entry> values;
    value_walker walker(doc);
    for (const auto& t : doc.tokens())
        walker.feed(t, values);
    return sorted(std::move(values));
}

auto value_fingerprints_of(const document& doc,
                           std::string_view element,
                           std::string_view key_attribute) -> std::vector<keyed_fingerprint> {
    std::vector<keyed_fingerprint> result;
    const auto& tokens = doc.tokens();
    for (std::size_t at = 0; at < tokens.size(); ++at) {
        const auto& t = tokens[at];
        if (t.kind != token_kind::begin_element || doc.name(t) != element)
            continue;

        // The occurrence ends with the end token that closes it, at the same
        // depth it began.
        auto end = at + 1;
        for (std::size_t depth = 1; end < tokens.size() && depth > 0; ++end) {
            if (tokens[end].kind == token_kind::begin_element)
                ++depth;
            else if (tokens[end].kind == token_kind::end_element)
                --depth;
        }
        const auto labels = sibling_labels(doc, at + 1, end);

        // The occurrence is walked on its own, so its paths start at the
        // element.
        keyed_fingerprint occurrence;
        value_walker walker(doc);
        walker.feed(t, occurrence.values);
        for (++at; at < end; ++at) {
            const auto& inner = tokens[at];
            if (inner.kind == token_kind::attribute && walker.depth() == 1 &&
                doc.name(inner) == key_attribute)
                occurrence.key = std::string(doc.value(inner));
            const auto label = labels.find(at);
            walker.feed(inner,
                        occurrence.values,
                        label == labels.end() ? std::string_view{} : label->second);
        }
        --at;
        occurrence.values = sorted(std::move(occurrence.values));
        result.push_back(std::move(occurrence));
    }
    return result;
}

}
