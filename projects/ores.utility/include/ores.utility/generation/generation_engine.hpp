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
#ifndef ORES_UTILITY_GENERATION_GENERATION_ENGINE_HPP
#define ORES_UTILITY_GENERATION_GENERATION_ENGINE_HPP

#include <array>
#include <chrono>
#include <cstdint>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>

namespace ores::utility::generation {

/**
 * @brief Random generation engine with seed-controlled repeatability.
 *
 * Provides UUID v7 generation, random integers, booleans, element picking,
 * timestamps, and alphanumeric strings. All random values are derived from
 * a single mt19937_64 engine seeded at construction.
 */
class generation_engine final {
public:
    /**
     * @brief Constructs an engine with a specific seed.
     */
    explicit generation_engine(std::uint64_t seed);

    /**
     * @brief Constructs an engine with a random seed.
     */
    generation_engine();

    /**
     * @brief Returns the seed used for this engine.
     */
    std::uint64_t seed() const { return seed_; }

    /**
     * @brief Generates a random integer in [min, max].
     */
    int random_int(int min, int max);

    /**
     * @brief Generates a random boolean with specified probability of true.
     */
    bool random_bool(double probability = 0.5);

    /**
     * @brief Picks a random element from a vector.
     */
    template<typename T>
    const T& pick(const std::vector<T>& items) {
        if (items.empty()) {
            throw std::out_of_range("Cannot pick from an empty vector.");
        }
        std::uniform_int_distribution<std::size_t> dist(0, items.size() - 1);
        return items[dist(engine_)];
    }

    /**
     * @brief Picks a random element from an array.
     */
    template<typename T, std::size_t N>
    const T& pick(const std::array<T, N>& items) {
        static_assert(N > 0, "Cannot pick from an empty array.");
        std::uniform_int_distribution<std::size_t> dist(0, N - 1);
        return items[dist(engine_)];
    }

    /**
     * @brief Generates a UUID v7 based on the engine's random state.
     */
    boost::uuids::uuid generate_uuid();

    /**
     * @brief Generates a random timestamp within the past N years.
     */
    std::chrono::system_clock::time_point past_timepoint(int years_back = 3);

    /**
     * @brief Generates an alphanumeric string of specified length.
     */
    std::string alphanumeric(std::size_t length);

private:
    std::uint64_t seed_;
    std::mt19937_64 engine_;
};

}

#endif
