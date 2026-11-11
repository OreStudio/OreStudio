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

template <typename T>
void write_schema(json_value out);

template <typename T>
void write_object(json_value out) {
    out.set("type", "object");
    auto properties = out.add_object("properties");
    auto required = out.add_array("required");
    apply_fields(
        [&](const auto&... members) {
            (
                [&] {
                    using member_type =
                        std::remove_cvref_t<decltype(std::declval<T>().*members.member)>;
                    auto slot = properties.add_object(members.name);
                    if constexpr (is_optional<member_type>::value) {
                        write_schema<typename member_type::value_type>(slot);
                    } else {
                        required.push(members.name);
                        write_schema<member_type>(slot);
                    }
                }(),
                ...);
        },
        wire_fields(std::type_identity<T>{}));
}

template <typename T>
void write_schema(json_value out) {
    using value_type = std::remove_cvref_t<T>;
    if constexpr (is_optional<value_type>::value) {
        write_schema<typename value_type::value_type>(out);
    } else if constexpr (is_vector<value_type>::value) {
        out.set("type", "array");
        write_schema<typename value_type::value_type>(out.add_object("items"));
    } else if constexpr (is_map<value_type>::value) {
        out.set("type", "object");
        write_schema<typename value_type::mapped_type>(out.add_object("additionalProperties"));
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
        write_object<value_type>(out);
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
    json_schema_detail::write_schema<std::remove_cvref_t<T>>(document.root());
    return document.write();
}

}

#endif
