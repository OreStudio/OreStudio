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

#include <optional>
#include <string_view>
#include <type_traits>

namespace ores::utility::codec {

/**
 * @brief One member of a struct, named as it is on the wire.
 *
 * A type states its own shape as a list of these, in the function the carriers
 * find by argument-dependent lookup -- =wire_fields=. The shape of an object is
 * therefore written once, beside the struct that has it, and the encoding is
 * written once, in the two carriers.
 *
 * This header holds no encoder: a type's own header can state its field list
 * without pulling yyjson or MessagePack behind it.
 */
template <typename T, typename Member>
struct field {
    std::string_view name;
    Member T::*member;
};

/**
 * @brief Whether a member's value may be absent.
 *
 * The two formats disagree about such a member, and the wire is reflect-cpp's
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
concept states_wire_form = requires(const T& v) { wire_fields(v); };

}

#endif
