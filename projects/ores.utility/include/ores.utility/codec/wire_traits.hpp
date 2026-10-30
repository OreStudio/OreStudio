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
#ifndef ORES_UTILITY_CODEC_WIRE_TRAITS_HPP
#define ORES_UTILITY_CODEC_WIRE_TRAITS_HPP

#include <boost/throw_exception.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::utility::codec {

/**
 * @brief How a type reaches the wire, in each format the wire states.
 *
 * The NATS codec is a template over the message type, so it cannot name a
 * generated function: it names this instead, and a generated codec specialises
 * this in the type's own translation unit. The primary template below keeps
 * reflect-cpp, which is what every type uses until its own codec is generated;
 * step 7 of the story deletes it, and with it the last reflect-cpp dependency.
 *
 * A specialisation states the four functions and nothing else, so the seam is
 * one place rather than one per call site.
 */
template <typename T>
struct wire_traits {
    static std::string write_json(const T& v) {
        return rfl::json::write(v);
    }

    static T read_json(std::string_view text) {
        auto read = rfl::json::read<T>(std::string(text));
        if (!read)
            throw std::invalid_argument(read.error().what());
        return *read;
    }

    static std::string write_msgpack(const T& v) {
        const auto bytes = rfl::msgpack::write(v);
        return std::string(bytes.begin(), bytes.end());
    }

    static T read_msgpack(std::span<const std::byte> data) {
        auto read = rfl::msgpack::read<T>(data);
        if (!read)
            throw std::invalid_argument(read.error().what());
        return *read;
    }
};

}

#endif
