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
#include "ores.nats/domain/wire_format.hpp"
#include <algorithm>
#include <cctype>
#include <utility>

namespace ores::nats {

namespace {

std::string to_lower(std::string_view s) {
    std::string r(s);
    std::ranges::transform(r, r.begin(), [](unsigned char c) { return std::tolower(c); });
    return r;
}

}

std::optional<wire_format> parse_wire_format(std::string_view name) {
    const auto lowered = to_lower(name);
    if (lowered == "json")
        return wire_format::json;
    if (lowered == "msgpack")
        return wire_format::msgpack;
    return std::nullopt;
}

std::string to_string(wire_format format) {
    switch (format) {
        case wire_format::json:
            return "json";
        case wire_format::msgpack:
            return "msgpack";
    }
    std::unreachable();
}

}
