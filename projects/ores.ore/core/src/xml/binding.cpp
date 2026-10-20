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
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/lexical.hpp"
#include "ores.ore.core/xml/scanner.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <array>
#include <charconv>
#include <expected>
#include <optional>
#include <vector>

namespace ores::ore::xml {

namespace {

/// The value record_type::text_field holds when a type has no text of its own.
constexpr auto hash_name(std::string_view name) -> std::uint32_t {
    std::uint32_t h = 2166136261u;
    for (const char c : name) {
        h ^= static_cast<std::uint8_t>(c);
        h *= 16777619u;
    }
    return h;
}

/**
 * @brief A linear probe over the type's fields.
 *
 * A schema type has a few dozen children at most, and the generator knows the
 * whole set, so a perfect hash is a change to the table rather than to this.
 */
auto find(const record_type& type, std::string_view name, bool attribute) -> const field* {
    const auto h = hash_name(name);
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        if (f.name_hash == h && f.name == name && f.is_attribute == attribute)
            return &f;
    }
    return nullptr;
}

/**
 * @brief The field that captures content the schema does not name, if any.
 */
auto residual_field(const record_type& type) -> const field* {
    for (std::uint8_t i = 0; i < type.field_count; ++i)
        if (type.fields[i].kind == field_kind::residual)
            return &type.fields[i];
    return nullptr;
}

/**
 * @brief The field a name denotes, whether the schema writes it as an element
 * or as an attribute.
 *
 * A projection names a member the way the document does, and the document
 * does not say which of the two it is. The generator refuses a type that
 * spells an element and an attribute the same, so the search cannot be
 * ambiguous.
 */
auto find_by_name(const record_type& type, std::string_view name) -> const field* {
    const auto* f = find(type, name, false);
    return f != nullptr ? f : find(type, name, true);
}

auto present(const void* record, const field& f) -> bool {
    return bit_is_set(record, f);
}

auto mark_present(void* record, const field& f) -> void {
    set_bit(record, f);
}

/**
 * @brief Writes one value into the member the field names.
 *
 * The caller hands over the element's whole text. A comment or a CDATA
 * boundary splits the text into runs, and the runs are one value, so they are
 * concatenated before this point and a number split that way is the number its
 * text states.
 */
auto store(void* record, const field& f, std::string_view text, std::string& error) -> bool {
    auto* slot = static_cast<std::byte*>(record) + f.offset;
    switch (f.kind) {
        case field_kind::text:
            *reinterpret_cast<std::string*>(slot) = text;
            break;
        case field_kind::integer: {
            const auto digits = trim_space(text);
            std::int64_t value = 0;
            const auto [stop, ec] =
                std::from_chars(digits.data(), digits.data() + digits.size(), value);
            if (ec != std::errc{} || stop != digits.data() + digits.size()) {
                error = "an element declared as an integer does not hold one";
                return false;
            }
            *reinterpret_cast<std::int64_t*>(slot) = value;
            break;
        }
        case field_kind::decimal: {
            const auto parsed = utility::decimal::decimal::from_string(trim_space(text));
            if (!parsed) {
                error = parsed.error();
                return false;
            }
            *reinterpret_cast<utility::decimal::decimal*>(slot) = *parsed;
            break;
        }
        case field_kind::real: {
            const auto digits = trim_space(text);
            double value = 0;
            const auto [stop, ec] =
                std::from_chars(digits.data(), digits.data() + digits.size(), value);
            if (ec != std::errc{} || stop != digits.data() + digits.size()) {
                error = "an element declared as a real does not hold one";
                return false;
            }
            *reinterpret_cast<double*>(slot) = value;
            break;
        }
        case field_kind::enumeration: {
            // ORE's currencyCode enumeration has more than a hundred
            // spellings, so an eight-bit index overflows and walks off the end
            // of the table. Sixteen bits is the floor here.
            std::int16_t index = -1;
            for (std::int16_t i = 0; f.tokens[i] != nullptr; ++i) {
                if (text == f.tokens[i]) {
                    index = i;
                    break;
                }
            }
            if (index < 0) {
                error = "an element declared as an enumeration holds no known spelling";
                return false;
            }
            *reinterpret_cast<std::int16_t*>(slot) = index;
            break;
        }
        case field_kind::repeated_leaf: {
            switch (f.scalar) {
                case scalar_kind::text:
                    reinterpret_cast<std::vector<std::string>*>(slot)->emplace_back(text);
                    break;
                case scalar_kind::integer: {
                    const auto digits = trim_space(text);
                    std::int64_t value = 0;
                    const auto [stop, ec] =
                        std::from_chars(digits.data(), digits.data() + digits.size(), value);
                    if (ec != std::errc{} || stop != digits.data() + digits.size()) {
                        error = "a repeated integer element does not hold one";
                        return false;
                    }
                    reinterpret_cast<std::vector<std::int64_t>*>(slot)->push_back(value);
                    break;
                }
                case scalar_kind::decimal: {
                    const auto parsed = utility::decimal::decimal::from_string(trim_space(text));
                    if (!parsed) {
                        error = parsed.error();
                        return false;
                    }
                    reinterpret_cast<std::vector<utility::decimal::decimal>*>(slot)->push_back(
                        *parsed);
                    break;
                }
                case scalar_kind::real: {
                    const auto digits = trim_space(text);
                    double value = 0;
                    const auto [stop, ec] =
                        std::from_chars(digits.data(), digits.data() + digits.size(), value);
                    if (ec != std::errc{} || stop != digits.data() + digits.size()) {
                        error = "a repeated real element does not hold one";
                        return false;
                    }
                    reinterpret_cast<std::vector<double>*>(slot)->push_back(value);
                    break;
                }
                case scalar_kind::enumeration: {
                    std::int16_t index = -1;
                    for (std::int16_t i = 0; f.tokens[i] != nullptr; ++i) {
                        if (text == f.tokens[i]) {
                            index = i;
                            break;
                        }
                    }
                    if (index < 0) {
                        error = "a repeated enumerated element holds no known spelling";
                        return false;
                    }
                    reinterpret_cast<std::vector<std::int16_t>*>(slot)->push_back(index);
                    break;
                }
            }
            break;
        }
        case field_kind::nested:
        case field_kind::repeated_nested:
        case field_kind::indexed_nested:
        case field_kind::column_nested:
            error = "a row describing a nested element reached the value writer";
            return false;
        case field_kind::residual:
            error = "a row describing unnamed content reached the value writer";
            return false;
    }
    mark_present(record, f);
    return true;
}

}

auto find_field(const record_type& type, std::string_view name, bool attribute) -> const field* {
    return find(type, name, attribute);
}

auto enumeration_spelling(const record_type& type, const void* record, std::string_view name)
    -> std::optional<std::string> {
    const auto* f = find_by_name(type, name);
    if (f == nullptr || f->kind != field_kind::enumeration || !present(record, *f) ||
        f->tokens == nullptr)
        return std::nullopt;
    const auto index =
        *reinterpret_cast<const std::int16_t*>(static_cast<const std::byte*>(record) + f->offset);
    if (index < 0)
        return std::nullopt;
    for (std::int16_t i = 0; i <= index; ++i)
        if (f->tokens[i] == nullptr)
            return std::nullopt;
    return std::string(f->tokens[index]);
}

auto set_enumeration(const record_type& type,
                     void* record,
                     std::string_view name,
                     std::string_view spelling) -> bool {
    const auto* f = find_by_name(type, name);
    if (f == nullptr || f->kind != field_kind::enumeration || f->tokens == nullptr)
        return false;
    for (std::int16_t i = 0; f->tokens[i] != nullptr; ++i) {
        if (spelling == f->tokens[i]) {
            *reinterpret_cast<std::int16_t*>(static_cast<std::byte*>(record) + f->offset) = i;
            mark_present(record, *f);
            return true;
        }
    }
    return false;
}

auto mark_text_present(const record_type& type, void* record) -> bool {
    if (type.text_field == no_text)
        return false;
    mark_present(record, type.fields[type.text_field]);
    return true;
}

auto find_child_type(const record_type& type, std::string_view name) -> const record_type* {
    const auto* f = find(type, name, false);
    return f != nullptr ? f->type : nullptr;
}

auto is_present(const record_type& type, const void* record, std::string_view name) -> bool {
    const auto* f = find_by_name(type, name);
    return f != nullptr && present(record, *f);
}

auto mark_present(const record_type& type, void* record, std::string_view name) -> bool {
    const auto* f = find_by_name(type, name);
    if (f == nullptr)
        return false;
    mark_present(record, *f);
    return true;
}

auto binder::bind(const document_type& doc, std::string_view text, void* root)
    -> std::expected<void, bind_error> {
    // A frame is either a record being filled or a leaf awaiting its text. A
    // leaf is a frame rather than a flag so a leaf with children is refused
    // instead of quietly popping its parent, and so a leaf's own attributes
    // resolve against the leaf rather than against whatever contains it.
    struct frame {
        const record_type* type;
        std::byte* record;
        const field* leaf;
        /// The element's text so far. A comment or a CDATA boundary splits it
        /// into runs and the runs are one value, so they are joined here and
        /// stored once when the element closes.
        std::string text;
        /// The columns struct of the record, when its type has columns.
        std::byte* columns = nullptr;
    };

    std::array<frame, max_depth> stack{};
    std::uint8_t depth = 0;
    bool have_root = false;

    // Content the schema does not name, while it is being captured. The text
    // is kept on the residual record and a child is refused.
    std::string* residual_text = nullptr;
    bool residual_had_children = false;
    std::string residual_name;
    int residual_depth = 0;

    reader input(text);

    auto fail = [&input](std::string message) -> std::expected<void, bind_error> {
        return std::unexpected(bind_error{std::move(message), input.line(), input.column()});
    };

    // Where the record a member holds keeps its columns, or null.
    auto columns_for = [](const frame& holder, const field& f) -> std::byte* {
        return f.columns_offset == no_arena ? nullptr : holder.record + f.columns_offset;
    };

    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::error)
            return std::unexpected(
                bind_error{std::string(input.error()), input.line(), input.column()});
        if (ev.kind == event_type::end_of_document) {
            if (!have_root)
                return fail("the document has no root element");
            if (depth != 0)
                return fail("the document ended inside an element");
            return {};
        }

        if (residual_depth > 0) {
            if (ev.kind == event_type::begin_element) {
                ++residual_depth;
                residual_had_children = true;
            } else if (ev.kind == event_type::text && residual_text != nullptr) {
                residual_text->append(ev.value);
            } else if (ev.kind == event_type::end_element) {
                if (--residual_depth == 0 && residual_had_children)
                    return fail("unknown element '" + residual_name +
                                "' has children, which a projection cannot represent");
            }
            continue;
        }

        switch (ev.kind) {
            case event_type::begin_element: {
                if (depth == 0) {
                    if (have_root)
                        return fail("the document has more than one root element");
                    if (ev.name != doc.root_name)
                        return fail("the root element is not the one this binding names");
                    have_root = true;
                    stack[depth++] = {doc.root, static_cast<std::byte*>(root), nullptr, {}};
                    continue;
                }
                const auto& top = stack[depth - 1];
                if (top.type == nullptr)
                    return fail("a leaf element cannot contain '" + std::string(ev.name) + "'");
                const auto* f = find(*top.type, ev.name, false);
                if (f == nullptr) {
                    const auto* any = residual_field(*top.type);
                    if (any == nullptr)
                        return fail("unknown element '" + std::string(ev.name) + "' in '" +
                                    std::string(top.type->name) + "'");
                    auto* bag =
                        reinterpret_cast<std::vector<residual_element>*>(top.record + any->offset);
                    bag->emplace_back();
                    bag->back().name = std::string(ev.name);
                    mark_present(top.record, *any);
                    residual_text = &bag->back().text;
                    residual_had_children = false;
                    residual_name = std::string(ev.name);
                    residual_depth = 1;
                    continue;
                }
                if (depth == max_depth)
                    return fail("the document nests deeper than the binding follows");
                auto* slot = top.record + f->offset;
                if (f->kind == field_kind::nested) {
                    // A nested member is a value inside its parent, so it is
                    // already constructed and is destroyed with the parent.
                    mark_present(top.record, *f);
                    stack[depth++] = {f->type, slot, nullptr, {}, columns_for(top, *f)};
                    continue;
                }
                if (f->kind == field_kind::column_nested) {
                    // The row holds one alternative of a choice: its presence
                    // bit says which, and its index says where the alternative
                    // sits in the column its holder owns.
                    if (top.columns == nullptr || f->repeated == nullptr)
                        return fail("an alternative of '" + std::string(top.type->name) +
                                    "' names no column");
                    for (std::uint8_t i = 0; i < top.type->field_count; ++i) {
                        const auto& other = top.type->fields[i];
                        if (other.kind == field_kind::column_nested &&
                            bit_is_set(top.record, other))
                            return fail("element '" + std::string(ev.name) +
                                        "' follows another alternative of '" +
                                        std::string(top.type->name) + "', which holds one");
                    }
                    auto* rows = top.columns + f->arena_offset;
                    const auto index = static_cast<std::uint32_t>(f->repeated->count(rows));
                    void* child = nullptr;
                    f->repeated->append(rows, &child);
                    *reinterpret_cast<std::uint32_t*>(slot) = index;
                    mark_present(top.record, *f);
                    stack[depth++] = {
                        f->type, static_cast<std::byte*>(child), nullptr, {}, nullptr};
                    continue;
                }
                if (f->kind == field_kind::indexed_nested) {
                    // A recursive member is an index into the arena its parent
                    // owns. The row is appended first, so a reallocation cannot
                    // move a row an open frame points at: while this frame is
                    // open the binder appends only to the child's own arena.
                    auto* indices = reinterpret_cast<std::vector<std::uint32_t>*>(slot);
                    if (f->repeated == nullptr || f->arena_offset == no_arena)
                        return fail("an indexed member of '" + std::string(top.type->name) +
                                    "' names no arena");
                    if (f->once && !indices->empty())
                        return fail("element '" + std::string(ev.name) +
                                    "' appears more than "
                                    "once in '" +
                                    std::string(top.type->name) + "'");
                    auto* arena = top.record + f->arena_offset;
                    const auto index = static_cast<std::uint32_t>(f->repeated->count(arena));
                    void* child = nullptr;
                    f->repeated->append(arena, &child);
                    indices->push_back(index);
                    mark_present(top.record, *f);
                    auto* record = static_cast<std::byte*>(child);
                    stack[depth++] = {f->type, record, nullptr, {}, columns_for(top, *f)};
                    continue;
                }
                if (f->kind == field_kind::repeated_nested) {
                    void* child = nullptr;
                    f->repeated->append(slot, &child);
                    mark_present(top.record, *f);
                    auto* record = static_cast<std::byte*>(child);
                    stack[depth++] = {f->type, record, nullptr, {}, columns_for(top, *f)};
                    continue;
                }
                // The bit means "this element appeared", not "it carried
                // text". ORE writes <Symbol></Symbol> for a required element
                // with no value, and a bit set only on text would report that
                // as missing.
                mark_present(top.record, *f);
                stack[depth++] = {nullptr, top.record, f, {}};
                continue;
            }
            case event_type::attribute: {
                if (depth == 0)
                    return fail("an attribute arrived outside the root element");
                const auto& top = stack[depth - 1];
                if (top.type == nullptr)
                    return fail("an element that holds a value cannot carry the attribute '" +
                                std::string(ev.name) + "'");
                const auto* f = find(*top.type, ev.name, true);
                if (f == nullptr)
                    return fail("unknown attribute '" + std::string(ev.name) + "' in '" +
                                std::string(top.type->name) + "'");
                std::string message;
                if (!store(top.record, *f, ev.value, message))
                    return fail(std::move(message));
                continue;
            }
            case event_type::text: {
                if (depth == 0) {
                    if (is_blank(ev.value))
                        continue;
                    return fail("text arrived outside the root element");
                }
                auto& top = stack[depth - 1];
                const bool holds_text = top.type == nullptr || top.type->text_field != no_text;
                if (!holds_text) {
                    // Beside a record's children, whitespace is formatting.
                    if (is_blank(ev.value))
                        continue;
                    return fail("text where no field expects it, inside '" +
                                std::string(top.type->name) + "'");
                }
                // A leaf and a simpleContent record both keep every run: the
                // runs are one value, and a blank run before a CDATA section is
                // part of it.
                top.text.append(ev.value);
                continue;
            }
            case event_type::end_element: {
                if (depth == 0)
                    return fail("an end tag arrived with nothing open");
                const auto& top = stack[depth - 1];
                // A blank-only text is the empty value an element with no text
                // states: a text field keeps it and an enumeration may spell it.
                const auto value =
                    is_blank(top.text) ? std::string_view{} : std::string_view(top.text);
                if (top.type == nullptr) {
                    std::string message;
                    if (!store(top.record, *top.leaf, value, message))
                        return fail(std::move(message));
                    --depth;
                    continue;
                }
                if (top.type->text_field != no_text) {
                    const auto& target = top.type->fields[top.type->text_field];
                    // A number cannot state an empty value, so an absent number
                    // stays absent rather than becoming a default zero.
                    const bool empty = value.empty();
                    const bool states_empty =
                        target.kind == field_kind::text || target.kind == field_kind::enumeration;
                    if (!empty || states_empty) {
                        std::string message;
                        if (!store(top.record, target, value, message))
                            return fail(std::move(message));
                        mark_present(top.record, target);
                    }
                }
                // An attribute is checked at the end of the element that
                // carries it, exactly like a child: both are content the
                // schema requires, and a missing one is a broken document
                // rather than an optional value.
                for (std::uint8_t i = 0; i < top.type->field_count; ++i) {
                    const auto& f = top.type->fields[i];
                    if (!f.required || f.kind == field_kind::residual)
                        continue;
                    if (!present(top.record, f))
                        return fail("required element '" + std::string(f.name) +
                                    "' is missing from '" + std::string(top.type->name) + "'");
                }
                --depth;
                continue;
            }
            default:
                continue;
        }
    }
}

}
