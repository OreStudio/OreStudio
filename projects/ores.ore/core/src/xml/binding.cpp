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
#include "ores.ore.core/xml/scanner.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <array>
#include <charconv>
#include <vector>

namespace ores::ore::xml {

namespace {

/// The value record_type::text_field holds when a type has no text of its own.
constexpr std::uint8_t no_text = 0xFF;

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

auto present(const void* record, const field& f) -> bool {
    return (*static_cast<const std::uint64_t*>(record) & (1ull << f.bit)) != 0;
}

auto mark_present(void* record, const field& f) -> void {
    *static_cast<std::uint64_t*>(record) |= (1ull << f.bit);
}

/**
 * @brief Writes one value into the member the field names.
 *
 * `append` says whether a second value for the same field joins the first.
 * Text accumulates, because a comment or a CDATA boundary splits a text node
 * into runs and the value is still one string. Every other kind replaces, and
 * a second value for a number is refused rather than silently overwriting the
 * first.
 */
auto store(void* record,
           const field& f,
           std::string_view text,
           bool append,
           std::string& error) -> bool {
    static_cast<void>(append);
    auto* slot = static_cast<std::byte*>(record) + f.offset;
    switch (f.kind) {
        case field_kind::text: {
            auto* target = reinterpret_cast<std::string*>(slot);
            if (append)
                target->append(text);
            else
                *target = text;
            break;
        }
        case field_kind::integer: {
            std::int64_t value = 0;
            const auto [stop, ec] = std::from_chars(text.data(), text.data() + text.size(), value);
            if (ec != std::errc{} || stop != text.data() + text.size()) {
                error = "an element declared as an integer does not hold one";
                return false;
            }
            *reinterpret_cast<std::int64_t*>(slot) = value;
            break;
        }
        case field_kind::decimal: {
            const auto parsed = utility::decimal::decimal::from_string(text);
            if (!parsed) {
                error = parsed.error();
                return false;
            }
            *reinterpret_cast<utility::decimal::decimal*>(slot) = *parsed;
            break;
        }
        case field_kind::real: {
            double value = 0;
            const auto [stop, ec] = std::from_chars(text.data(), text.data() + text.size(), value);
            if (ec != std::errc{} || stop != text.data() + text.size()) {
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
                    std::int64_t value = 0;
                    const auto [stop, ec] =
                        std::from_chars(text.data(), text.data() + text.size(), value);
                    if (ec != std::errc{} || stop != text.data() + text.size()) {
                        error = "a repeated integer element does not hold one";
                        return false;
                    }
                    reinterpret_cast<std::vector<std::int64_t>*>(slot)->push_back(value);
                    break;
                }
                case scalar_kind::decimal: {
                    const auto parsed = utility::decimal::decimal::from_string(text);
                    if (!parsed) {
                        error = parsed.error();
                        return false;
                    }
                    reinterpret_cast<std::vector<utility::decimal::decimal>*>(slot)->push_back(
                        *parsed);
                    break;
                }
                case scalar_kind::real: {
                    double value = 0;
                    const auto [stop, ec] =
                        std::from_chars(text.data(), text.data() + text.size(), value);
                    if (ec != std::errc{} || stop != text.data() + text.size()) {
                        error = "a repeated real element does not hold one";
                        return false;
                    }
                    reinterpret_cast<std::vector<double>*>(slot)->push_back(value);
                    break;
                }
            }
            break;
        }
        case field_kind::nested:
        case field_kind::repeated_nested:
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

auto binder::bind(const document_type& doc, std::string_view text, void* root, bind_error& error)
    -> bool {
    // A frame is either a record being filled or a leaf awaiting its text. A
    // leaf is a frame rather than a flag so a leaf with children is refused
    // instead of quietly popping its parent, and so a leaf's own attributes
    // resolve against the leaf rather than against whatever contains it.
    struct frame {
        const record_type* type;
        std::byte* record;
        const field* leaf;
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

    auto fail = [&input, &error](std::string message) {
        error.message = std::move(message);
        error.line = input.line();
        error.column = input.column();
        return false;
    };

    for (;;) {
        const auto ev = input.next();
        if (ev.kind == event_type::error) {
            error.message = std::string(input.error());
            error.line = input.line();
            error.column = input.column();
            return false;
        }
        if (ev.kind == event_type::end_of_document) {
            if (!have_root)
                return fail("the document has no root element");
            if (depth != 0)
                return fail("the document ended inside an element");
            return true;
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
                    stack[depth++] = {doc.root, static_cast<std::byte*>(root), nullptr};
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
                    auto* bag = reinterpret_cast<std::vector<residual_element>*>(top.record +
                                                                                any->offset);
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
                    stack[depth++] = {f->type, slot, nullptr};
                    if (f->type->text_field != no_text) {
                        auto* target = &f->type->fields[f->type->text_field];
                        mark_present(slot, *target);
                    }
                    continue;
                }
                if (f->kind == field_kind::repeated_nested) {
                    void* child = nullptr;
                    f->append(slot, &child);
                    mark_present(top.record, *f);
                    auto* record = static_cast<std::byte*>(child);
                    stack[depth++] = {f->type, record, nullptr};
                    if (f->type->text_field != no_text) {
                        auto* target = &f->type->fields[f->type->text_field];
                        mark_present(record, *target);
                    }
                    continue;
                }
                // The bit means "this element appeared", not "it carried
                // text". ORE writes <Symbol></Symbol> for a required element
                // with no value, and a bit set only on text would report that
                // as missing.
                mark_present(top.record, *f);
                stack[depth++] = {nullptr, top.record, f};
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
                if (!store(top.record, *f, ev.value, false, message))
                    return fail(std::move(message));
                continue;
            }
            case event_type::text: {
                if (is_blank(ev.value))
                    continue;
                if (depth == 0)
                    return fail("text arrived outside the root element");
                const auto& top = stack[depth - 1];
                std::string message;
                if (top.type == nullptr) {
                    if (!store(top.record, *top.leaf, ev.value, true, message))
                        return fail(std::move(message));
                    continue;
                }
                if (top.type->text_field == no_text)
                    return fail("text where no field expects it, inside '" +
                                std::string(top.type->name) + "'");
                const auto& target = top.type->fields[top.type->text_field];
                if (!store(top.record, target, ev.value, true, message))
                    return fail(std::move(message));
                continue;
            }
            case event_type::end_element: {
                if (depth == 0)
                    return fail("an end tag arrived with nothing open");
                const auto& top = stack[depth - 1];
                if (top.type == nullptr) {
                    --depth;
                    continue;
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
