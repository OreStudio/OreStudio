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

// A document codec driven by descriptor tables.
//
// The tables are data: one array per record layout saying which element name
// writes to which member, with which value kind. One engine walks the tables,
// so adding a field is a line in a table, never a line of code. Reading and
// writing share the same tables, which keeps the two directions from drifting.

#include "scanner.hpp"

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>
#include <memory>
#include <new>

namespace ore::xml {

enum class value_kind : std::uint8_t { text, integer, enumeration, decimal_text };

// `offset` is produced by offsetof in the generated table, so the table and
// the record cannot disagree about where a field lives.
struct field {
    std::string_view name;
    std::uint32_t name_hash;
    value_kind kind;
    std::uint16_t offset;
    std::uint8_t bit;
    const char* const* tokens;
};

struct record_type {
    std::string_view name;
    const field* fields;
    std::uint8_t field_count;
};

struct document_type {
    std::string_view root_name;
    std::string_view item_name;
    const record_type* item;
};

// A record layout. `present` is the first member of every layout because the
// engine sets its bits by field index, which keeps an absent value apart from
// an empty one without an optional wrapper or a heap allocation.
struct currency_row {
    std::uint16_t present = 0;
    std::string name;
    std::string iso_code;
    std::string minor_unit_codes;
    std::string symbol;
    std::string fraction_symbol;
    std::string currency_type;
    std::int64_t numeric_code = 0;
    std::int64_t fractions_per_unit = 0;
    std::int64_t rounding_precision = 0;
    std::int8_t rounding_type = -1;
};

// The same layout with every value borrowed from the document buffer. Nothing
// here outlives the buffer, and nothing here allocates.
struct currency_view {
    std::uint16_t present = 0;
    std::string_view name;
    std::string_view iso_code;
    std::string_view minor_unit_codes;
    std::string_view symbol;
    std::string_view fraction_symbol;
    std::string_view currency_type;
    std::int64_t numeric_code = 0;
    std::int64_t fractions_per_unit = 0;
    std::int64_t rounding_precision = 0;
    std::int8_t rounding_type = -1;
};

struct result {
    bool ok = true;
    std::string_view message;
    std::uint32_t line = 0;
    std::uint32_t column = 0;
};

auto currency_document() -> const document_type&;
auto currency_view_document() -> const document_type&;
auto write_currencies(const std::vector<currency_row>& rows, std::string& out) -> void;

namespace detail {

constexpr auto hash_name(std::string_view name) -> std::uint32_t {
    std::uint32_t h = 2166136261u;
    for (const char c : name) {
        h ^= static_cast<std::uint8_t>(c);
        h *= 16777619u;
    }
    return h;
}

inline auto find_field(const record_type& type, std::string_view name) -> const field* {
    const auto h = hash_name(name);
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.name_hash == h && f.name == name)
            return &f;
    }
    return nullptr;
}

inline auto set_present(std::byte* record, const field& f) -> void {
    *reinterpret_cast<std::uint16_t*>(record) |= static_cast<std::uint16_t>(1u << f.bit);
}

inline auto is_present(const std::byte* record, const field& f) -> bool {
    return (*reinterpret_cast<const std::uint16_t*>(record) & (1u << f.bit)) != 0;
}

inline auto parse_integer(std::string_view text, std::int64_t& out) -> bool {
    std::size_t i = 0;
    while (i < text.size() && (text[i] == ' ' || text[i] == '\t' || text[i] == '\n' ||
                               text[i] == '\r'))
        ++i;
    bool negative = false;
    if (i < text.size() && (text[i] == '-' || text[i] == '+'))
        negative = text[i++] == '-';
    if (i >= text.size() || text[i] < '0' || text[i] > '9')
        return false;
    std::uint64_t magnitude = 0;
    for (; i < text.size() && text[i] >= '0' && text[i] <= '9'; ++i)
        magnitude = magnitude * 10 + static_cast<std::uint64_t>(text[i] - '0');
    out = negative ? -static_cast<std::int64_t>(magnitude) : static_cast<std::int64_t>(magnitude);
    return true;
}

inline auto format_integer(std::int64_t value, char* buffer) -> std::string_view {
    char* end = buffer;
    if (value == 0) {
        *end++ = '0';
        return {buffer, 1};
    }
    const bool negative = value < 0;
    std::uint64_t magnitude =
        negative ? (~static_cast<std::uint64_t>(value) + 1) : static_cast<std::uint64_t>(value);
    char digits[20];
    int count = 0;
    while (magnitude != 0) {
        digits[count++] = static_cast<char>('0' + magnitude % 10);
        magnitude /= 10;
    }
    if (negative)
        *end++ = '-';
    while (count > 0)
        *end++ = digits[--count];
    return {buffer, static_cast<std::size_t>(end - buffer)};
}

// A policy names the record layout and says how one value lands in it. Both
// policies share the engine and the tables; only these six functions differ.
struct materialized {
    using row = currency_row;

    static auto set_text(std::byte* record, const field& f, std::string_view value) -> void {
        *reinterpret_cast<std::string*>(record + f.offset) = value;
    }
    static auto set_integer(std::byte* record, const field& f, std::int64_t value) -> void {
        *reinterpret_cast<std::int64_t*>(record + f.offset) = value;
    }
    static auto set_enumeration(std::byte* record, const field& f, std::int64_t value) -> void {
        *reinterpret_cast<std::int8_t*>(record + f.offset) = static_cast<std::int8_t>(value);
    }
    static auto text(const std::byte* record, const field& f) -> std::string_view {
        return *reinterpret_cast<const std::string*>(record + f.offset);
    }
    static auto integer(const std::byte* record, const field& f) -> std::int64_t {
        return *reinterpret_cast<const std::int64_t*>(record + f.offset);
    }
    static auto enumeration(const std::byte* record, const field& f) -> std::int64_t {
        return *reinterpret_cast<const std::int8_t*>(record + f.offset);
    }
};

struct borrowed {
    using row = currency_view;

    static auto set_text(std::byte* record, const field& f, std::string_view value) -> void {
        *reinterpret_cast<std::string_view*>(record + f.offset) = value;
    }
    static auto set_integer(std::byte* record, const field& f, std::int64_t value) -> void {
        *reinterpret_cast<std::int64_t*>(record + f.offset) = value;
    }
    static auto set_enumeration(std::byte* record, const field& f, std::int64_t value) -> void {
        *reinterpret_cast<std::int8_t*>(record + f.offset) = static_cast<std::int8_t>(value);
    }
    static auto text(const std::byte* record, const field& f) -> std::string_view {
        return *reinterpret_cast<const std::string_view*>(record + f.offset);
    }
    static auto integer(const std::byte* record, const field& f) -> std::int64_t {
        return *reinterpret_cast<const std::int64_t*>(record + f.offset);
    }
    static auto enumeration(const std::byte* record, const field& f) -> std::int64_t {
        return *reinterpret_cast<const std::int8_t*>(record + f.offset);
    }
};

template <typename Policy>
auto store_value(const field& f, std::byte* record, std::string_view text) -> result {
    switch (f.kind) {
        case value_kind::text:
        case value_kind::decimal_text:
            Policy::set_text(record, f, text);
            break;
        case value_kind::integer: {
            std::int64_t value = 0;
            if (!parse_integer(text, value))
                return {false, "an element declared as an integer does not hold one"};
            Policy::set_integer(record, f, value);
            break;
        }
        case value_kind::enumeration: {
            std::int8_t index = -1;
            for (std::int8_t i = 0; f.tokens[i] != nullptr; ++i)
                if (text == f.tokens[i])
                    index = i;
            if (index < 0)
                return {false, "an element declared as an enumeration holds no known spelling"};
            Policy::set_enumeration(record, f, index);
            break;
        }
    }
    set_present(record, f);
    return {};
}

template <typename Policy, typename Sink>
auto read_items(std::string_view document, const document_type& doc, Sink&& sink) -> result {
    reader input(document);
    alignas(typename Policy::row) std::byte buffer[sizeof(typename Policy::row)];
    using row_type = typename Policy::row;

    bool in_item = false;
    bool skipping = false;
    int skip_depth = 0;
    const field* target = nullptr;

    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::error)
            return {false, input.error(), input.line(), input.column()};
        if (ev.kind == event_type::end_of_document)
            return in_item ? result{false, "a document ended inside an item"}
                           : result{};
        if (ev.kind == event_type::attribute)
            continue;

        if (skipping) {
            if (ev.kind == event_type::begin_element)
                ++skip_depth;
            else if (ev.kind == event_type::end_element && --skip_depth == 0)
                skipping = false;
            continue;
        }

        if (ev.kind == event_type::begin_element) {
            target = nullptr;
            if (!in_item) {
                if (ev.name == doc.item_name) {
                    new (buffer) row_type{};
                    in_item = true;
                    continue;
                }
                skipping = ev.name != doc.root_name;
                skip_depth = skipping ? 1 : 0;
                continue;
            }
            const auto* f = find_field(*doc.item, ev.name);
            if (!f) {
                skipping = true;
                skip_depth = 1;
                continue;
            }
            target = f;
            continue;
        }

        if (ev.kind == event_type::text) {
            if (target != nullptr) {
                const auto stored = store_value<Policy>(*target, buffer, ev.value);
                if (!stored.ok)
                    return {false, stored.message, input.line(), input.column()};
                target = nullptr;
            }
            continue;
        }

        target = nullptr;
        if (in_item && ev.name == doc.item_name) {
            auto* record = reinterpret_cast<row_type*>(buffer);
            sink(*record);
            std::destroy_at(record);
            in_item = false;
        }
    }
}

template <typename Policy>
auto write_items(const std::vector<currency_row>& rows,
                 const document_type& doc,
                 std::string& out) -> void {
    out.clear();
    out.reserve(rows.size() * 160);
    out.append("<").append(doc.root_name).append(">\n");
    for (const auto& r : rows) {
        const auto* record = reinterpret_cast<const std::byte*>(&r);
        out.append("  <").append(doc.item_name).append(">\n");
        for (std::uint8_t i = 0; i < doc.item->field_count; ++i) {
            const auto& f = doc.item->fields[i];
            if (!is_present(record, f))
                continue;
            out.append("    <").append(f.name).append(">");
            switch (f.kind) {
                case value_kind::text:
                case value_kind::decimal_text:
                    out.append(Policy::text(record, f));
                    break;
                case value_kind::integer: {
                    char buffer[24];
                    const auto digits = format_integer(Policy::integer(record, f), buffer);
                    out.append(digits);
                    break;
                }
                case value_kind::enumeration:
                    out.append(f.tokens[Policy::enumeration(record, f)]);
                    break;
            }
            out.append("</").append(f.name).append(">\n");
        }
        out.append("  </").append(doc.item_name).append(">\n");
    }
    out.append("</").append(doc.root_name).append(">\n");
}

} // namespace detail

template <typename Sink>
auto read_currencies(std::string_view document, Sink&& sink) -> result {
    return detail::read_items<detail::materialized>(document, currency_document(), sink);
}

template <typename Sink>
auto read_currency_views(std::string_view document, Sink&& sink) -> result {
    return detail::read_items<detail::borrowed>(document, currency_view_document(), sink);
}

} // namespace ore::xml
