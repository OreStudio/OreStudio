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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_CODEC_JSON_SCHEMA_HPP
#define ORES_UTILITY_CODEC_JSON_SCHEMA_HPP

#include "ores.utility/codec/json_document.hpp"
#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <magic_enum/magic_enum.hpp>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::utility::codec {

/**
 * @file json_schema.hpp
 * @brief The JSON schema a stated shape implies.
 *
 * A route publishes the schema of the body it reads and the response it writes,
 * and the shape the codec walks is what states both: the same wire_fields that
 * drive the encoder say which members exist, what each one holds and whether it
 * may be absent. So the schema cannot drift from the bytes, because there is one
 * statement of the shape rather than two.
 *
 * A member that is not optional is required, because the reader refuses a
 * document that leaves it out. A member whose type is written as text -- a uuid,
 * a decimal, a timestamp, an enumeration -- is a string, and an enumeration
 * names the text each of its values has.
 */

namespace json_schema_detail {

template <typename T>
struct is_optional : std::false_type {};

template <typename T>
struct is_optional<std::optional<T>> : std::true_type {};

template <typename T>
struct is_vector : std::false_type {};

template <typename T>
struct is_vector<std::vector<T>> : std::true_type {};

template <typename T>
struct is_map : std::false_type {};

template <typename T>
struct is_map<std::map<std::string, T>> : std::true_type {};

/// A type whose JSON is one string, as the codec writes it.
template <typename T>
concept written_as_text = requires { sizeof(text_form<std::remove_cvref_t<T>>); };

/**
 * @brief The type's own name, as the compiler states it.
 *
 * A schema that a reader resolves by name needs a name, and the compiler is the only
 * place one exists: the wire shape says what the members are, not what the type is
 * called. The name is taken from the function's own signature, which is the one thing
 * both clang and gcc state the same way once the brackets are stripped.
 */
template <typename T>
constexpr std::string_view stated_name() {
    constexpr std::string_view signature = __PRETTY_FUNCTION__;
    const auto at = signature.find("T = ");
    if (at == std::string_view::npos)
        return {};
    const auto first = at + 4;
    const auto last = signature.find_first_of("];", first);
    return signature.substr(first,
                            (last == std::string_view::npos ? signature.size() : last) - first);
}

/**
 * @brief The name a definition is registered under: the type's name as a dotted path.
 *
 * The leading =ores= is dropped because every type in the document is one of its own,
 * and the rest is kept whole: a component whose name is its own path is traceable to the
 * type that writes it, and two types that differ only by namespace stay apart.
 */
template <typename T>
std::string definition_name() {
    auto name = std::string(stated_name<T>());
    if (name.starts_with("ores::"))
        name.erase(0, std::string("ores::").size());
    for (auto at = name.find("::"); at != std::string::npos; at = name.find("::", at + 1))
        name.replace(at, 2, ".");
    return name;
}

/// The definitions one document collects, and the names it already holds.
struct definitions final {
    json_value of;
    std::set<std::string> written;
};

template <typename T>
void write_schema(json_value out, definitions& defs);

template <typename T>
void write_object(json_value out, definitions& defs) {
    const auto name = definition_name<T>();
    if (name.empty()) {
        // A compiler that states no name still gets a schema; it just states it in place.
        out.set("type", "object");
        return;
    }
    out.set("$ref", "#/$defs/" + name);
    if (defs.written.contains(name))
        return;
    // Written before the members are walked, so a type that holds itself refers to itself.
    defs.written.insert(name);

    auto self = defs.of.add_object(name);
    self.set("type", "object");
    auto properties = self.add_object("properties");
    auto required = self.add_array("required");
    apply_fields(
        [&](const auto&... members) {
            (
                [&] {
                    using member_type =
                        std::remove_cvref_t<decltype(std::declval<T>().*members.member)>;
                    auto slot = properties.add_object(members.name);
                    if constexpr (is_optional<member_type>::value) {
                        // Written as null when empty, and may be left out of a body.
                        write_schema<typename member_type::value_type>(slot, defs);
                        slot.set("nullable", true);
                    } else {
                        required.push(members.name);
                        write_schema<member_type>(slot, defs);
                    }
                }(),
                ...);
        },
        wire_fields(std::type_identity<T>{}));
}

template <typename T>
void write_schema(json_value out, definitions& defs) {
    using value_type = std::remove_cvref_t<T>;
    if constexpr (is_optional<value_type>::value) {
        write_schema<typename value_type::value_type>(out, defs);
    } else if constexpr (is_vector<value_type>::value) {
        out.set("type", "array");
        write_schema<typename value_type::value_type>(out.add_object("items"), defs);
    } else if constexpr (is_map<value_type>::value) {
        out.set("type", "object");
        write_schema<typename value_type::mapped_type>(out.add_object("additionalProperties"),
                                                       defs);
    } else if constexpr (std::is_same_v<value_type, bool>) {
        out.set("type", "boolean");
    } else if constexpr (std::is_integral_v<value_type>) {
        out.set("type", "integer");
    } else if constexpr (std::is_floating_point_v<value_type>) {
        out.set("type", "number");
    } else if constexpr (std::is_enum_v<value_type>) {
        out.set("type", "string");
        auto names = out.add_array("enum");
        for (const auto name : magic_enum::enum_names<value_type>())
            names.push(name);
    } else if constexpr (written_as_text<value_type>) {
        out.set("type", "string");
    } else if constexpr (states_wire_form<value_type>) {
        write_object<value_type>(out, defs);
    } else {
        // A message whose shape lives in a codec header that is not in view
        // here still gets a schema: an object with no members stated, which is
        // valid and says less. The protocol headers that name such a message
        // include the codec headers of the types it holds, so this is the case
        // for a component whose regeneration lags its template.
        out.set("type", "object");
    }
}

}

/**
 * @brief The JSON schema the type's shape implies.
 */
template <typename T>
[[nodiscard]] std::string json_schema() {
    auto document = json_document::make_object();
    auto definitions = json_document::make_object();
    auto state = json_schema_detail::definitions{definitions.root(), {}};
    json_schema_detail::write_schema<std::remove_cvref_t<T>>(document.root(), state);
    // A shape with no named type inside it states no definitions at all.
    if (definitions.root().size() > 0)
        document.root().add("$defs", definitions.root());
    return document.write();
}

}

#endif
