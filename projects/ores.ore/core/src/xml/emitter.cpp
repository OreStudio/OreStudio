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
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <array>
#include <charconv>
#include <cstddef>
#include <string_view>

namespace ores::ore::xml {

namespace {

/// The deepest nesting this writer follows. It is the binder's limit, so a
/// document the writer emits binds again.
constexpr std::size_t max_emit_depth = binder::max_depth;

constexpr std::string_view declaration = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
constexpr std::string_view indent_unit = "  ";

auto present(const void* record, const field& f) -> bool {
    return bit_is_set(record, f);
}

auto write_indent(std::string& out, std::size_t depth) -> void {
    out.push_back('\n');
    for (std::size_t i = 0; i < depth; ++i)
        out.append(indent_unit);
}

/**
 * @brief Appends one text node or attribute value, escaping what must be.
 *
 * One scan decides it. A document that needs no escaping is copied whole,
 * which is the common case: an ORE name or a date carries none of these
 * characters. An attribute value escapes the whitespace a parser would
 * normalize away, so the value that leaves is the value that returns.
 */
auto append_escaped(std::string& out, std::string_view text, bool attribute) -> void {
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
            case '"':
                if (attribute)
                    out.append("&quot;");
                else
                    out.push_back(c);
                break;
            case '\n':
                if (attribute)
                    out.append("&#10;");
                else
                    out.push_back(c);
                break;
            case '\r':
                if (attribute)
                    out.append("&#13;");
                else
                    out.push_back(c);
                break;
            case '\t':
                if (attribute)
                    out.append("&#9;");
                else
                    out.push_back(c);
                break;
            default:
                out.push_back(c);
                break;
        }
    }
}

auto append_integer(std::string& out, std::int64_t value) -> void {
    std::array<char, 32> buffer{};
    const auto [stop, ec] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    if (ec == std::errc{})
        out.append(buffer.data(), stop);
}

/**
 * @brief Appends one continuous quantity as the shortest text that reads back
 * to the same double.
 */
auto append_real(std::string& out, double value) -> void {
    std::array<char, 64> buffer{};
    const auto [stop, ec] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    if (ec == std::errc{})
        out.append(buffer.data(), stop);
}

/**
 * @brief Appends one spelling of an enumeration, which the row carries.
 */
auto append_spelling(std::string& out,
                     const field& f,
                     std::int16_t index) -> std::expected<void, emit_error> {
    if (index < 0 || f.tokens == nullptr)
        return std::unexpected(emit_error{"an enumeration holds no spelling"});
    for (std::int16_t i = 0; i <= index; ++i)
        if (f.tokens[i] == nullptr)
            return std::unexpected(
                emit_error{"an enumeration index is past the end of its spellings"});
    append_escaped(out, f.tokens[index], false);
    return {};
}

/**
 * @brief Appends one value of a repeated leaf, which its row names the type of.
 */
auto append_scalar(std::string& out,
                   const field& f,
                   const void* slot) -> std::expected<void, emit_error> {
    switch (f.scalar) {
        case scalar_kind::text:
            append_escaped(out, *static_cast<const std::string*>(slot), false);
            return {};
        case scalar_kind::integer:
            append_integer(out, *static_cast<const std::int64_t*>(slot));
            return {};
        case scalar_kind::decimal:
            out.append(static_cast<const utility::decimal::decimal*>(slot)->to_string());
            return {};
        case scalar_kind::real:
            append_real(out, *static_cast<const double*>(slot));
            return {};
        case scalar_kind::enumeration:
            return append_spelling(out, f, *static_cast<const std::int16_t*>(slot));
    }
    return std::unexpected(emit_error{"a repeated leaf of an unknown kind reached the writer"});
}

auto slot_of(const void* record, const field& f) -> const void* {
    return static_cast<const std::byte*>(record) + f.offset;
}

/**
 * @brief Appends one value of a leaf row.
 *
 * @param f The row the value belongs to, which names the type it holds.
 * @param slot The member's address.
 */
auto append_value(std::string& out,
                  const field& f,
                  const void* slot) -> std::expected<void, emit_error> {
    switch (f.kind) {
        case field_kind::text:
            append_escaped(out, *static_cast<const std::string*>(slot), false);
            return {};
        case field_kind::integer:
            append_integer(out, *static_cast<const std::int64_t*>(slot));
            return {};
        case field_kind::decimal:
            out.append(static_cast<const utility::decimal::decimal*>(slot)->to_string());
            return {};
        case field_kind::real:
            append_real(out, *static_cast<const double*>(slot));
            return {};
        case field_kind::enumeration:
            return append_spelling(out, f, *static_cast<const std::int16_t*>(slot));
        case field_kind::nested:
        case field_kind::repeated_leaf:
        case field_kind::repeated_nested:
        case field_kind::indexed_nested:
        case field_kind::residual:
            return std::unexpected(emit_error{"a row that names no single value reached the "
                                              "value writer"});
    }
    return std::unexpected(emit_error{"a row of an unknown kind reached the value writer"});
}

auto write_element(std::string& out,
                   const record_type& type,
                   const void* record,
                   std::size_t depth,
                   std::string_view name) -> std::expected<void, emit_error>;

auto write_indexed(std::string& out,
                   const field& f,
                   const void* record,
                   std::size_t depth) -> std::expected<void, emit_error> {
    if (f.repeated == nullptr || f.type == nullptr || f.arena_offset == no_arena)
        return std::unexpected(emit_error{"an indexed member names no arena"});
    const auto* indices = static_cast<const std::vector<std::uint32_t>*>(slot_of(record, f));
    if (f.once && indices->size() > 1)
        return std::unexpected(
            emit_error{"an element the schema allows once is indexed more than once"});
    const auto* arena = static_cast<const std::byte*>(record) + f.arena_offset;
    const auto rows = f.repeated->count(arena);
    std::vector<bool> named(rows, false);
    for (const auto index : *indices) {
        if (index >= rows)
            return std::unexpected(
                emit_error{"an indexed member names a row the arena does not hold"});
        named[index] = true;
        const auto written =
            write_element(out, *f.type, f.repeated->at(arena, index), depth, f.name);
        if (!written)
            return written;
    }
    // A row no index names has no place in a document, and writing nothing for
    // it would lose it without a word.
    for (const auto seen : named)
        if (!seen)
            return std::unexpected(
                emit_error{"an indexed member's arena holds a row no index names"});
    return {};
}

auto write_element(std::string& out,
                   const record_type& type,
                   const void* record,
                   std::size_t depth,
                   std::string_view name) -> std::expected<void, emit_error>;

/**
 * @brief Appends one repeated member's rows, each under its own element.
 */
auto write_repeated(std::string& out,
                    const field& f,
                    const void* record,
                    std::size_t depth) -> std::expected<void, emit_error> {
    const auto* slot = slot_of(record, f);
    const auto count = f.repeated->count(slot);
    for (std::size_t i = 0; i < count; ++i) {
        const void* element = f.repeated->at(slot, i);
        if (f.kind == field_kind::repeated_nested) {
            const auto written = write_element(out, *f.type, element, depth, f.name);
            if (!written)
                return written;
            continue;
        }
        write_indent(out, depth);
        out.push_back('<');
        out.append(f.name);
        out.push_back('>');
        const auto written = append_scalar(out, f, element);
        if (!written)
            return written;
        out.append("</");
        out.append(f.name);
        out.push_back('>');
    }
    return {};
}

auto write_element(std::string& out,
                   const record_type& type,
                   const void* record,
                   std::size_t depth,
                   std::string_view name) -> std::expected<void, emit_error> {
    // The binder holds max_depth frames, so an element this writer places at
    // that depth would not bind again.
    if (depth >= max_emit_depth)
        return std::unexpected(emit_error{"the document nests deeper than the binder follows"});
    write_indent(out, depth);
    out.push_back('<');
    out.append(name);

    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (!f.is_attribute)
            continue;
        if (!present(record, f)) {
            if (f.required)
                return std::unexpected(emit_error{"required attribute '" + std::string(f.name) +
                                                  "' is absent from '" + std::string(type.name) +
                                                  "'"});
            continue;
        }
        out.push_back(' ');
        out.append(f.name);
        out.append("=\"");
        append_escaped(out, *static_cast<const std::string*>(slot_of(record, f)), true);
        out.push_back('"');
    }

    bool has_children = false;
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.is_attribute || i == type.text_field)
            continue;
        if (f.required && !present(record, f))
            return std::unexpected(emit_error{"required element '" + std::string(f.name) +
                                              "' is absent from '" + std::string(type.name) + "'"});
        if (!present(record, f))
            continue;
        switch (f.kind) {
            case field_kind::residual:
                has_children =
                    has_children ||
                    !static_cast<const std::vector<residual_element>*>(slot_of(record, f))->empty();
                break;
            case field_kind::indexed_nested:
                has_children =
                    has_children ||
                    !static_cast<const std::vector<std::uint32_t>*>(slot_of(record, f))->empty();
                break;
            case field_kind::repeated_leaf:
            case field_kind::repeated_nested:
                has_children = has_children || f.repeated->count(slot_of(record, f)) != 0;
                break;
            default:
                has_children = true;
                break;
        }
    }

    std::string own_text;
    if (type.text_field != no_text) {
        const auto& tf = type.fields[type.text_field];
        // A number the reading left absent states no value, and writing the
        // member's zero would invent one.
        if (present(record, tf)) {
            const auto written = append_value(own_text, tf, slot_of(record, tf));
            if (!written)
                return written;
        }
    }

    if (!has_children && own_text.empty()) {
        out.append("/>");
        return {};
    }
    out.push_back('>');
    if (!has_children) {
        out.append(own_text);
        out.append("</");
        out.append(name);
        out.push_back('>');
        return {};
    }

    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.is_attribute || i == type.text_field || !present(record, f))
            continue;
        switch (f.kind) {
            case field_kind::nested: {
                const auto written =
                    write_element(out, *f.type, slot_of(record, f), depth + 1, f.name);
                if (!written)
                    return written;
                break;
            }
            case field_kind::indexed_nested: {
                const auto written = write_indexed(out, f, record, depth + 1);
                if (!written)
                    return written;
                break;
            }
            case field_kind::repeated_nested:
            case field_kind::repeated_leaf: {
                const auto written = write_repeated(out, f, record, depth + 1);
                if (!written)
                    return written;
                break;
            }
            case field_kind::residual: {
                const auto* extras =
                    static_cast<const std::vector<residual_element>*>(slot_of(record, f));
                for (const auto& extra : *extras) {
                    write_indent(out, depth + 1);
                    out.push_back('<');
                    out.append(extra.name);
                    out.push_back('>');
                    append_escaped(out, extra.text, false);
                    out.append("</");
                    out.append(extra.name);
                    out.push_back('>');
                }
                break;
            }
            default: {
                write_indent(out, depth + 1);
                out.push_back('<');
                out.append(f.name);
                out.push_back('>');
                const auto written = append_value(out, f, slot_of(record, f));
                if (!written)
                    return written;
                out.append("</");
                out.append(f.name);
                out.push_back('>');
                break;
            }
        }
    }
    write_indent(out, depth);
    out.append("</");
    out.append(name);
    out.push_back('>');
    return {};
}

}

auto emitter::emit(const document_type& doc,
                   const void* root,
                   std::string& out) -> std::expected<void, emit_error> {
    out.clear();
    out.append(declaration);
    const auto written = write_element(out, *doc.root, root, 0, doc.root_name);
    if (!written) {
        out.clear();
        return written;
    }
    out.push_back('\n');
    return {};
}

}
