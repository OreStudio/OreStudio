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
#ifndef ORES_UTILITY_CODEC_WIRE_FIELDS_HPP
#define ORES_UTILITY_CODEC_WIRE_FIELDS_HPP

#include <cstddef>
#include <optional>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

namespace ores::utility::codec {

/**
 * @brief One member of a struct, named as it is on the wire.
 *
 * A type states its own shape as a list of these, in the function the carriers
 * find by argument-dependent lookup -- =wire_fields=. The shape of an object is
 * therefore written once, beside the struct that has it, and the encoding is
 * written once, in the two carriers.
 *
 * =wire_fields= takes the type's =std::type_identity= and not a value, because
 * the shape is a property of the type.
 *
 * This header holds no encoder: a type's own header can state its field list
 * without pulling yyjson or MessagePack behind it.
 */
template <typename T, typename Member>
struct field {
    std::string_view name;
    Member T::* member;
};

template <std::size_t I, typename Field>
struct field_slot {
    Field value;
};

template <typename Indices, typename... Fields>
struct field_list_storage;

template <std::size_t... Is, typename... Fields>
struct field_list_storage<std::index_sequence<Is...>, Fields...> : field_slot<Is, Fields>... {
    constexpr field_list_storage(Fields... fields)
        : field_slot<Is, Fields>{fields}... {}

    template <typename F>
    constexpr decltype(auto) apply(F&& f) const {
        return std::forward<F>(f)(static_cast<const field_slot<Is, Fields>&>(*this).value...);
    }
};

/**
 * @brief The fields of a type, in the order of its stated shape.
 *
 * Every type's header defines =wire_fields= and so builds one of these, in
 * every translation unit that includes it, whether the unit encodes the type
 * or not. =std::tuple= costs the compiler work that grows with the square of
 * the field count, because libstdc++ nests one base per element over the rest
 * of the pack and checks its constructor constraints at every level. This list
 * has one flat base per field, so its cost grows with the field count alone.
 */
template <typename... Fields>
struct field_list : field_list_storage<std::index_sequence_for<Fields...>, Fields...> {
    using field_list_storage<std::index_sequence_for<Fields...>, Fields...>::field_list_storage;

    static constexpr std::size_t size = sizeof...(Fields);
};

template <typename... Fields>
field_list(Fields...) -> field_list<Fields...>;

/**
 * @brief Calls @p f with every field of @p fields, as one pack.
 */
template <typename F, typename... Fields>
constexpr decltype(auto) apply_fields(F&& f, const field_list<Fields...>& fields) {
    return fields.apply(std::forward<F>(f));
}

/**
 * @brief Whether a member's value may be absent.
 *
 * The two formats disagree about such a member, and the wire fixes the
 * bytes in both, so the disagreement is stated instead of discovered: JSON
 * omits a member whose optional is empty, while MessagePack carries it as nil.
 * The readers accept an absent member either way, because a peer that omits it
 * and a peer that sends nil are saying the same thing.
 */
template <typename Member>
inline constexpr bool is_optional_member = false;

template <typename Member>
inline constexpr bool is_optional_member<std::optional<Member>> = true;

/**
 * @brief Whether a type states its own shape.
 *
 * The call is unqualified and its argument is dependent, so it resolves by
 * argument-dependent lookup when it is instantiated: a type whose header states
 * =wire_fields= answers yes, and every other type -- a string, a number, a
 * uuid, a container -- answers no and keeps the form its own carrier gives it.
 */
template <typename T>
concept states_wire_form = requires { wire_fields(std::type_identity<T>{}); };

/**
 * @brief Calls @p f with the name and the value of each member of @p v.
 *
 * The members come in the order of the type's stated shape. A member is const
 * when @p v is const.
 */
template <typename T, typename F>
    requires states_wire_form<std::remove_const_t<T>>
constexpr void for_each_field(T& v, F&& f) {
    apply_fields([&](const auto&... fields) { (f(fields.name, v.*(fields.member)), ...); },
                 wire_fields(std::type_identity<std::remove_const_t<T>>{}));
}

}

#endif
