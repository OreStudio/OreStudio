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
#ifndef ORES_UTILITY_CODEC_MEMBER_COUNT_HPP
#define ORES_UTILITY_CODEC_MEMBER_COUNT_HPP

#include <cstddef>
#include <tuple>
#include <utility>

namespace ores::utility::codec {

namespace detail {

/**
 * @brief A value that converts to any member type, so that brace-initialising
 * an aggregate with N of them compiles exactly when it has at least N members.
 *
 * The conversion is declared and never defined: it is only named in an
 * unevaluated requirement.
 */
struct any_member {
    template <typename T>
    operator T() const noexcept;
};

template <typename T, typename... Seen>
consteval std::size_t count_members() {
    if constexpr (requires { T{Seen{}..., any_member{}}; })
        return count_members<T, Seen..., any_member>();
    else
        return sizeof...(Seen);
}

}

/**
 * @brief The number of members an aggregate declares.
 *
 * It counts by brace-initialising the type, so it holds for an aggregate
 * without C-array members: brace elision would let an array take several
 * initialisers and the count would be too high.
 */
template <typename T>
inline constexpr std::size_t member_count = detail::count_members<T>();

/**
 * @brief Whether a hand-written shape names every member of its type.
 *
 * A generated shape is complete by construction. A hand-written one is not, and
 * a member added to the struct but not to its =wire_fields= is silently absent
 * from the wire. A header that states a shape by hand asserts this beside it,
 * naming how many members it leaves out on purpose, such as a password.
 *
 * The members left out are a count, not names: a shape that left out the wrong
 * member and named the password would still pass. The guard catches a member
 * forgotten, not one swapped.
 */
template <typename T>
consteval bool names_every_member(std::size_t left_out = 0) {
    return member_count<T> ==
           std::tuple_size_v<decltype(wire_fields(std::declval<const T&>()))> + left_out;
}

}

#endif
