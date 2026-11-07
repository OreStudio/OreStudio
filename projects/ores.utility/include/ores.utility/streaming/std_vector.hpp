/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_UTILITY_STREAMING_STD_VECTOR_HPP
#define ORES_UTILITY_STREAMING_STD_VECTOR_HPP

#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/rfl/time_point_parser.hpp"
#include "ores.utility/streaming/tenant_id_parser.hpp"
#include "ores.utility/streaming/uuid_parser.hpp"
#include <cstddef>
#include <ostream>
#include <vector>

namespace ores::utility::streaming {

template <typename Containee>
concept codec_writes_whole = ores::utility::codec::states_wire_form<Containee> ||
                             requires { sizeof(ores::utility::codec::text_form<Containee>); };

template <typename Containee>
concept prints_itself = requires(std::ostream& s, const Containee& c) { s << c; };

}

namespace std {


/*
 * A type the codec can write whole is written as one JSON array. Any other
 * element is written by its own operator<<, which for a generated domain type
 * writes the same JSON through its compiled codec, so the caller need not have
 * the element's shape in view. A vector of anything else has no operator<< here,
 * so a test framework falls back to its own range printer.
 */
template <typename Containee>
    requires(ores::utility::streaming::codec_writes_whole<Containee> ||
             ores::utility::streaming::prints_itself<Containee>)
inline ostream& operator<<(ostream& s, const vector<Containee>& v) {
    if constexpr (ores::utility::streaming::codec_writes_whole<Containee>) {
        s << ores::utility::codec::json::write(v);
    } else {
        s << '[';
        for (std::size_t i = 0; i < v.size(); ++i)
            s << (i == 0 ? "" : ",") << v[i];
        s << ']';
    }
    return s;
}

}

#endif
