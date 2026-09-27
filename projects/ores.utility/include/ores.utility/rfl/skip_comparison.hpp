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
#ifndef ORES_UTILITY_RFL_SKIP_COMPARISON_HPP
#define ORES_UTILITY_RFL_SKIP_COMPARISON_HPP

#include <rfl/Skip.hpp>

namespace rfl::internal {

/**
 * @brief Equality for a skipped field, so a generated struct can default it.
 *
 * A domain column the model marks =:no_wire:= keeps its member and is skipped
 * when the struct is serialized, which is how a credential stays in C++ and
 * leaves the wire. The generated struct defaults its equality operator, and
 * rfl::Skip carries the value and marks it for the serializers but defines no
 * comparison, so that defaulted operator would be ill-formed. Compare the
 * underlying values, which is what the member means.
 *
 * The operators live in =rfl::internal= because that is the namespace the type
 * is declared in, which is where argument-dependent lookup searches. Remove
 * this shim if a later reflect-cpp defines the comparison itself.
 */
template <class T, bool SkipSerialization, bool SkipDeserialization>
constexpr bool
operator==(const Skip<T, SkipSerialization, SkipDeserialization>& lhs,
           const Skip<T, SkipSerialization, SkipDeserialization>& rhs) {
    return lhs.get() == rhs.get();
}

template <class T, bool SkipSerialization, bool SkipDeserialization>
constexpr bool
operator!=(const Skip<T, SkipSerialization, SkipDeserialization>& lhs,
           const Skip<T, SkipSerialization, SkipDeserialization>& rhs) {
    return !(lhs == rhs);
}

} // namespace rfl::internal

#endif
