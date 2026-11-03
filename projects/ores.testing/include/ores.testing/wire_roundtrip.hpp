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
#ifndef ORES_TESTING_WIRE_ROUNDTRIP_HPP
#define ORES_TESTING_WIRE_ROUNDTRIP_HPP

#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/sample.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <span>
#include <string>

namespace ores::testing {

/**
 * @brief Requires a type's wire codec to read back exactly what it wrote.
 *
 * It checks JSON and MessagePack, for a sample that sets every member away from
 * its default and for the default value. The sample fills only the members the
 * type's wire_fields names, so the guard first requires that list to name every
 * member: a member left off it would stay at its default, and the roundtrip
 * would read the default back and pass.
 */
template <typename T>
void check_wire_roundtrip() {
    static_assert(utility::codec::names_every_member<T>(0),
                  "the type has a member its wire shape does not name");
    using traits = utility::codec::wire_traits<T>;
    const auto bytes = [](const std::string& s) {
        return std::span<const std::byte>(reinterpret_cast<const std::byte*>(s.data()), s.size());
    };
    const auto check = [&](const T& v) {
        CHECK(traits::read_json(traits::write_json(v)) == v);
        CHECK(traits::read_msgpack(bytes(traits::write_msgpack(v))) == v);
    };
    {
        INFO("sample value");
        check(utility::codec::sample<T>(0));
    }
    {
        INFO("default value");
        check(T{});
    }
}

}

#endif
