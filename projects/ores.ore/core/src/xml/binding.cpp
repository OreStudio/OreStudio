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
    return (*static_cast<const std::uint16_t*>(record) & (1u << f.bit)) != 0;
}

auto mark_present(void* record, const field& f) -> void {
    *static_cast<std::uint16_t*>(record) |= static_cast<std::uint16_t>(1u << f.bit);
}

auto store(void* record, const field& f, std::string_view text, std::string& error) -> bool {
    auto* slot = static_cast<std::byte*>(record) + f.offset;
    switch (f.kind) {
        case field_kind::text:
            *reinterpret_cast<std::string*>(slot) = text;
            break;
        case field_kind::integer: {
            std::int64_t value = 0;
            const auto begin = text.data();
            const auto end = begin + text.size();
            const auto [stop, ec] = std::from_chars(begin, end, value);
            if (ec != std::errc{} || stop != end) {
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
            const auto begin = text.data();
            const auto end = begin + text.size();
            const auto [stop, ec] = std::from_chars(begin, end, value);
            if (ec != std::errc{} || stop != end) {
                error = "an element declared as a real does not hold one";
                return false;
            }
            *reinterpret_cast<double*>(slot) = value;
            break;
        }
        case field_kind::enumeration: {
            // ORE's currencyCode enumeration has more than a hundred
            // spellings, so an eight-bit index overflows and walks off the
            // end of the table. Sixteen bits is the floor here, not a choice.
            std::int16_t index = -1;
            for (std::int16_t i = 0; f.tokens[i] != nullptr; ++i)
                if (text == f.tokens[i])
                    index = i;
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
                    const auto [stop, ec] = std::from_chars(text.data(), text.data() + text.size(),
                                                            value);
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
                    const auto [stop, ec] = std::from_chars(text.data(), text.data() + text.size(),
                                                            value);
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
        case field_kind::residual:
            return false;
        case field_kind::nested:
        case field_kind::repeated_nested:
            return false;
    }
    mark_present(record, f);
    return true;
}

}

auto binder::bind(const document_type& doc, std::string_view text, void* root, bind_error& error)
    -> bool {
    struct frame {
        const record_type* type;
        std::byte* record;
    };

    std::array<frame, max_depth> stack{};
    std::uint8_t depth = 0;

    const field* pending = nullptr;
    std::byte* pending_record = nullptr;
    // A leaf has no children, so the next end tag closes it rather than the
    // frame below it. Without this the leaf's end would pop its parent.
    bool in_leaf = false;
    int skipping = 0;
    // Set while the engine is inside an element the schema does not name. Its
    // text is kept on the residual record and its children are refused.
    std::string* residual_text = nullptr;
    bool residual_had_children = false;
    std::string residual_name;

    reader input(text);

    auto fail = [&error](std::string message) {
        error.message = std::move(message);
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
            if (depth != 0)
                return fail("the document ended inside an element");
            return true;
        }

        if (skipping > 0) {
            if (ev.kind == event_type::begin_element) {
                ++skipping;
                residual_had_children = true;
            } else if (ev.kind == event_type::text && residual_text != nullptr) {
                residual_text->append(ev.value);
            } else if (ev.kind == event_type::end_element) {
                if (--skipping == 0 && residual_had_children) {
                    error.message = "unknown element '" + residual_name +
                                    "' has children, which a projection cannot represent";
                    error.line = input.line();
                    error.column = input.column();
                    return false;
                }
            }
            continue;
        }

        switch (ev.kind) {
            case event_type::begin_element: {
                if (depth == 0) {
                    if (ev.name != doc.root_name) {
                        error.message = "the root element is not the one this binding names";
                        error.line = input.line();
                        error.column = input.column();
                        return false;
                    }
                    stack[depth++] = {doc.root, static_cast<std::byte*>(root)};
                    continue;
                }
                const auto& top = stack[depth - 1];
                const auto* f = find(*top.type, ev.name, false);
                if (f == nullptr) {
                    // Content the schema does not name goes to the residual
                    // field when the type declares one. A type without one
                    // refuses the document, because a projection that ignores
                    // data cannot be checked against the source.
                    const auto* any = residual_field(*top.type);
                    if (any == nullptr) {
                        error.message = "unknown element '" + std::string(ev.name) + "' in '" +
                                        std::string(top.type->name) + "'";
                        error.line = input.line();
                        error.column = input.column();
                        return false;
                    }
                    auto* bag = reinterpret_cast<std::vector<residual_element>*>(top.record +
                                                                                any->offset);
                    bag->emplace_back();
                    bag->back().name = std::string(ev.name);
                    mark_present(top.record, *any);
                    residual_text = &bag->back().text;
                    residual_had_children = false;
                    residual_name = std::string(ev.name);
                    skipping = 1;
                    continue;
                }
                if (depth == max_depth)
                    return fail("the document nests deeper than the binding follows");
                auto* slot = top.record + f->offset;
                if (f->kind == field_kind::nested) {
                    // A nested member is a value inside its parent, so it is
                    // already constructed and is destroyed with the parent.
                    mark_present(top.record, *f);
                    stack[depth++] = {f->type, slot};
                    if (f->type->text_field != no_text) {
                        // xs:simpleContent: the element carries text of its
                        // own beside its attributes.
                        pending = &f->type->fields[f->type->text_field];
                        pending_record = slot;
                        mark_present(pending_record, *pending);
                    }
                    continue;
                }
                if (f->kind == field_kind::repeated_nested) {
                    void* child = nullptr;
                    f->append(slot, &child);
                    mark_present(top.record, *f);
                    auto* record = static_cast<std::byte*>(child);
                    stack[depth++] = {f->type, record};
                    if (f->type->text_field != no_text) {
                        pending = &f->type->fields[f->type->text_field];
                        pending_record = record;
                        mark_present(pending_record, *pending);
                    }
                    continue;
                }
                // The bit means "this element appeared", not "it carried
                // text". ORE writes <Symbol></Symbol> for a required element
                // with no value, and a bit set only on text would report that
                // as missing.
                mark_present(top.record, *f);
                in_leaf = true;
                pending = f;
                pending_record = top.record;
                continue;
            }
            case event_type::attribute: {
                if (depth == 0)
                    return fail("an attribute arrived outside the root element");
                const auto& top = stack[depth - 1];
                const auto* f = find(*top.type, ev.name, true);
                if (f == nullptr) {
                    error.message = "unknown attribute '" + std::string(ev.name) + "' in '" +
                                    std::string(top.type->name) + "'";
                    error.line = input.line();
                    error.column = input.column();
                    return false;
                }
                std::string message;
                if (!store(top.record, *f, ev.value, message))
                    return fail(std::move(message));
                continue;
            }
            case event_type::text: {
                if (is_blank(ev.value))
                    continue;
                if (pending == nullptr) {
                    // Text the model has no field for is data, and dropping it
                    // would be the loss this layer exists to prevent. Mixed
                    // content and an unknown shape both land here.
                    error.message = "text where no field expects it";
                    if (depth > 0)
                        error.message += " inside '" +
                                         std::string(stack[depth - 1].type->name) + "'";
                    error.line = input.line();
                    error.column = input.column();
                    return false;
                }
                std::string message;
                if (!store(pending_record, *pending, ev.value, message))
                    return fail(std::move(message));
                pending = nullptr;
                pending_record = nullptr;
                continue;
            }
            case event_type::end_element: {
                pending = nullptr;
                pending_record = nullptr;
                residual_text = nullptr;
                if (in_leaf) {
                    in_leaf = false;
                    continue;
                }
                if (depth == 0)
                    return fail("an end tag arrived with nothing open");
                --depth;
                const auto& top = stack[depth];
                for (std::uint8_t i = 0; i < top.type->field_count; ++i) {
                    const auto& f = top.type->fields[i];
                    // An attribute is checked at the end of the element that
                    // carries it, exactly like a child: both are content the
                    // schema requires, and a missing one is a broken document
                    // rather than an optional value. The repeated kinds set
                    // their bit when they append, so minOccurs=1 works too.
                    if (!f.required || f.kind == field_kind::residual)
                        continue;
                    if (!present(top.record, f)) {
                        error.message = "required element '" + std::string(f.name) +
                                        "' is missing from '" + std::string(top.type->name) + "'";
                        error.line = input.line();
                        error.column = input.column();
                        return false;
                    }
                }
                continue;
            }
            default:
                continue;
        }
    }
}

}
