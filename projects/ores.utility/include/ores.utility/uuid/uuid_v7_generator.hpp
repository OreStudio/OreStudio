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
 * FOR A PARTICULAR PURPOSE. Seethe GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General PublicLicense along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_UUID_UUID_V7_GENERATOR_HPP
#define ORES_UTILITY_UUID_UUID_V7_GENERATOR_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <random>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>

namespace ores::utility::uuid {

/**
 * @class uuid_v7_generator
 * @brief A generator for UUID version 7 (v7) based on RFC 9562.
 *
 * @details This class creates UUIDs that are time-sortable and contain
 * high-entropy random data. A UUID v7 is composed of a 48-bit Unix timestamp
 * (in milliseconds), a 4-bit version field, and 74 bits of random data.
 *
 * The combination of a timestamp and random data makes v7 UUIDs excellent for
 * use as primary keys in distributed databases, as they are sortable by
 * generation time and have a very low probability of collision.
 *
 * This implementation is designed to be efficient, creating and seeding a
 * random number engine only once upon construction.
 *
 * @note C++23 does not include a standard UUID library, and Boost.UUID does not
 * provide a v7 generator. This class fills that gap.
 *
 * @see <a
 * href="https://www.rfc-editor.org/rfc/rfc9562.html#name-uuid-version-7">RFC
 * 9562 - UUID Version 7</a>
 */
class uuid_v7_generator {
public:
    /**
     * @brief Constructs a new UUID v7 generator.
     *
     * @details Initializes the internal Mersenne Twister random number engine
     * with a high-entropy seed obtained from `std::random_device`. This ensures
     * that the random components of the generated UUIDs are unpredictable.
     */
    uuid_v7_generator() : random_engine(std::random_device{}()) {}

    /**
     * @brief Generates a new UUID v7.
     *
     * @details This function performs the core generation logic:
     * 1. Retrieves the current time as a 48-bit Unix timestamp in milliseconds.
     * 2. Generates a base UUID filled with cryptographically strong random bytes.
     * 3. Overwrites the timestamp, version, and variant fields within the
     *    random base to conform to the v7 specification.
     *
     * The resulting UUID is globally unique, time-sortable, and suitable for
     * a wide range of applications.
     *
     * @return A `boost::uuids::uuid` object conforming to the v7 specification.
     *
     * @note The 48-bit timestamp will overflow around the year 10889, which is
     * generally not a concern for most systems.
     */
    boost::uuids::uuid operator()();

private:
    /**
     * @var std::mt19937_64 uuid_v7_generator::random_engine
     * @brief The Mersenne Twister 19937 64-bit random number engine.
     *
     * @details This engine is seeded once upon construction and reused for all
     * subsequent calls to `operator()()`. This approach is more efficient than
     * creating a new engine for each UUID and provides high-quality random data
     * for the UUID's random fields.
     */
    std::mt19937_64 random_engine;
};

}

#endif
